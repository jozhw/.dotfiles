#!/usr/bin/env python3
"""
Org to Starlight Markdown Converter

Converts an Org (literate config) file into a set of Markdown files that follow
the Astro Starlight content format. Every top-level Org heading (``*``) becomes
its own ``.md`` page with proper YAML frontmatter; nested headings (``**``,
``***`` ...) become ``##``/``###`` headings within that page.

Design notes / fixes over the original implementation:

* Inline ``=code=``/``~verbatim~`` and links are *protected* before any
  emphasis substitution runs, so URLs like ``https://a.com/b/c`` are no longer
  shredded by the italic (``/.../``) rule.
* Emphasis rules use word-boundary guards, so file paths (``/usr/local``) and
  the like are left alone.
* Org property drawers (``:PROPERTIES:`` ... ``:END:``) and stray ``#+`` keyword
  lines are dropped instead of leaking into the page body.
* Internal links (``[[*Heading][text]]`` and ``[[id:UUID][text]]``) cannot be
  resolved across the per-page split, so they degrade gracefully to their
  description text rather than emitting a broken anchor.
* ``#+begin_src`` blocks are dedented and stripped of surrounding blank lines
  for clean fenced output.
* Frontmatter values are YAML-quoted/escaped, and a description is derived from
  the section's first paragraph when one is available.
"""

import argparse
import re
from pathlib import Path
from typing import Dict, List, Optional

# Maximum length for an auto-generated description.
MAX_DESCRIPTION_LEN = 160

# Placeholder templates used to shield spans from emphasis substitution.
_PROTECT_RE = re.compile(r"\x00(\d+)\x00")


def sanitize_filename(title: str) -> str:
    """Convert a heading title into a stable, slug-style filename stem."""
    filename = re.sub(r"[^\w\s-]", "", title)
    filename = re.sub(r"[-\s]+", "-", filename)
    return filename.strip("-").lower()


def _yaml_quote(value: str) -> str:
    """Return ``value`` as a safely double-quoted YAML scalar."""
    escaped = value.replace("\\", "\\\\").replace('"', '\\"')
    return f'"{escaped}"'


def convert_inline(text: str) -> str:
    """Convert inline Org markup on a single line to Markdown.

    Order matters: protected spans (code, verbatim, links) are extracted first
    so emphasis rules only ever see plain prose.
    """
    protected: List[str] = []

    def protect(payload: str) -> str:
        protected.append(payload)
        return f"\x00{len(protected) - 1}\x00"

    # Inline code: =code= and ~verbatim~ both render as Markdown backticks.
    text = re.sub(r"=([^=\n]+)=", lambda m: protect(f"`{m.group(1)}`"), text)
    text = re.sub(r"~([^~\n]+)~", lambda m: protect(f"`{m.group(1)}`"), text)

    # Links with a description: [[target][label]].
    def link_with_label(match: re.Match) -> str:
        target, label = match.group(1), match.group(2)
        if _is_internal_link(target):
            # Cross-page anchors are unreliable post-split; keep the label only.
            return protect(label)
        return protect(f"[{label}]({target})")

    text = re.sub(r"\[\[([^\]]+)\]\[([^\]]+)\]\]", link_with_label, text)

    # Bare links: [[target]].
    def bare_link(match: re.Match) -> str:
        target = match.group(1)
        if _is_internal_link(target):
            return protect(target.lstrip("*#"))
        return protect(f"<{target}>")

    text = re.sub(r"\[\[([^\]]+)\]\]", bare_link, text)

    # Bold: *text* -> **text** (guarded so paths/words are untouched).
    text = re.sub(r"(?<![\w*])\*(?!\s)([^*\n]+?)(?<!\s)\*(?![\w*])", r"**\1**", text)

    # Italic: /text/ -> *text* (guarded so URLs/paths are untouched).
    text = re.sub(r"(?<![\w/])/(?!\s)([^/\n]+?)(?<!\s)/(?![\w/])", r"*\1*", text)

    # Restore protected spans. A protected payload (e.g. a link label) may itself
    # contain a placeholder (e.g. inline code inside that label), so restore
    # repeatedly until the text stabilises.
    def restore(match: re.Match) -> str:
        return protected[int(match.group(1))]

    for _ in range(len(protected) + 1):
        if not _PROTECT_RE.search(text):
            break
        text = _PROTECT_RE.sub(restore, text)

    return text


def _is_internal_link(target: str) -> bool:
    """True for Org-internal targets that have no stable cross-page URL."""
    return target.startswith(("*", "#", "id:", "file:"))


def _dedent_and_trim(lines: List[str]) -> List[str]:
    """Remove common leading indentation and surrounding blank lines."""
    while lines and not lines[0].strip():
        lines.pop(0)
    while lines and not lines[-1].strip():
        lines.pop()
    indents = [len(ln) - len(ln.lstrip()) for ln in lines if ln.strip()]
    if indents:
        common = min(indents)
        lines = [ln[common:] if ln.strip() else "" for ln in lines]
    return lines


def parse_org_content(content: str) -> List[Dict]:
    """Parse Org content into a list of top-level sections."""
    sections: List[Dict] = []
    current: Optional[Dict] = None

    in_code = False
    code_lines: List[str] = []
    code_lang = ""
    in_drawer = False

    for line in content.split("\n"):
        stripped = line.strip()

        # --- Code blocks -------------------------------------------------
        if stripped.startswith("#+begin_src"):
            in_code = True
            parts = stripped.split()
            code_lang = parts[1] if len(parts) > 1 else ""
            code_lines = []
            continue
        if stripped.startswith("#+end_src"):
            in_code = False
            if current is not None:
                body = _dedent_and_trim(code_lines)
                current["content"].append(f"```{code_lang}")
                current["content"].extend(body)
                current["content"].append("```")
            code_lines = []
            continue
        if in_code:
            code_lines.append(line)
            continue

        # --- Property drawers --------------------------------------------
        if stripped == ":PROPERTIES:":
            in_drawer = True
            continue
        if in_drawer:
            if stripped == ":END:":
                in_drawer = False
            continue

        # --- Top-level heading: starts a new page ------------------------
        if re.match(r"\*\s", line) and not line.startswith("**"):
            if current is not None:
                sections.append(current)
            title = line[2:].strip()
            current = {
                "title": convert_inline(title),
                "filename": sanitize_filename(title),
                "content": [],
            }
            continue

        if current is None:
            continue

        # --- Nested headings ---------------------------------------------
        heading = re.match(r"(\*+)\s+(.*)$", line)
        if heading:
            level = len(heading.group(1))
            text = convert_inline(heading.group(2).strip())
            current["content"].append("#" * level + " " + text)
            continue

        # --- Skip remaining Org keyword lines ----------------------------
        if stripped.startswith("#+"):
            continue

        current["content"].append(convert_inline(line))

    if current is not None:
        sections.append(current)

    return sections


def _collapse_blank_lines(text: str) -> str:
    """Collapse 3+ consecutive newlines down to a single blank line."""
    return re.sub(r"\n{3,}", "\n\n", text).strip() + "\n"


def _derive_description(section: Dict) -> str:
    """Build a description from the section's first prose paragraph."""
    in_code = False
    for raw in section["content"]:
        line = raw.strip()
        if line.startswith("```"):
            in_code = not in_code
            continue
        if in_code:
            continue
        if not line or line.startswith(("#", "-", "+", "|")):
            continue
        # Strip inline markup leftovers for a clean summary.
        clean = re.sub(r"[`*_]", "", line)
        clean = re.sub(r"\[([^\]]+)\]\([^)]+\)", r"\1", clean)
        clean = re.sub(r"<([^>]+)>", r"\1", clean)
        clean = re.sub(r"\s+", " ", clean).strip()
        if not clean:
            continue
        if len(clean) > MAX_DESCRIPTION_LEN:
            clean = clean[:MAX_DESCRIPTION_LEN].rsplit(" ", 1)[0] + "…"
        return clean
    return f"Documentation for {section['title']}"


def render_markdown(section: Dict) -> str:
    """Render a section dict into a complete Markdown document."""
    title = section["title"]
    description = _derive_description(section)
    frontmatter = (
        "---\n"
        f"title: {_yaml_quote(title)}\n"
        f"description: {_yaml_quote(description)}\n"
        "---\n\n"
    )
    body = "\n".join(section["content"])
    return _collapse_blank_lines(frontmatter + body)


def convert_org_to_starlight(org_file: Path, output_dir: Path, clean: bool) -> int:
    """Convert ``org_file`` into Starlight Markdown pages under ``output_dir``."""
    content = org_file.read_text(encoding="utf-8")
    # Strip stray control characters (incl. NUL) so they cannot collide with the
    # internal placeholder sentinel or leak into the rendered pages.
    content = re.sub(r"[\x00-\x08\x0b\x0c\x0e-\x1f]", "", content)
    sections = [s for s in parse_org_content(content) if s["filename"]]

    output_dir.mkdir(parents=True, exist_ok=True)

    if clean:
        generated = {output_dir / f"{s['filename']}.md" for s in sections}
        for existing in output_dir.glob("*.md"):
            if existing not in generated:
                existing.unlink()
                print(f"Removed stale: {existing}")

    for section in sections:
        filepath = output_dir / f"{section['filename']}.md"
        filepath.write_text(render_markdown(section), encoding="utf-8")
        print(f"Created: {filepath}")

    print(f"\nConverted {len(sections)} sections from {org_file} to {output_dir}")
    return len(sections)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Convert an Org file to Astro Starlight Markdown pages."
    )
    parser.add_argument("input", help="Input .org file path")
    parser.add_argument(
        "-o", "--output", default="./docs", help="Output directory (default: ./docs)"
    )
    parser.add_argument(
        "--clean",
        action="store_true",
        help="Remove generated .md files in the output dir that are no longer produced",
    )
    args = parser.parse_args()

    input_file = Path(args.input)
    output_dir = Path(args.output)

    if not input_file.exists():
        print(f"Error: Input file '{input_file}' does not exist")
        return 1
    if input_file.suffix.lower() != ".org":
        print(f"Warning: Input file '{input_file}' does not have a .org extension")

    try:
        convert_org_to_starlight(input_file, output_dir, args.clean)
    except Exception as exc:  # noqa: BLE001 - surface any failure to the CLI
        print(f"Error during conversion: {exc}")
        return 1

    print(f"\nConversion complete! See '{output_dir}' for the generated pages.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
