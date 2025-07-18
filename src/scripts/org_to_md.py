#!/usr/bin/env python3
"""
Org to Starlight Markdown Converter

Converts Org files to Markdown files following the Starlight Astro template format.
Each main heading (#) becomes a separate markdown file with proper frontmatter.
"""

import os
import re
import argparse
from pathlib import Path
from typing import List, Dict, Tuple


def sanitize_filename(title: str) -> str:
    """Convert title to a valid filename."""
    # Remove or replace invalid characters
    filename = re.sub(r"[^\w\s-]", "", title)
    filename = re.sub(r"[-\s]+", "-", filename)
    return filename.strip("-").lower()


def parse_org_content(content: str) -> List[Dict]:
    """Parse org content and extract sections with their headings."""
    lines = content.split("\n")
    sections = []
    current_section = None
    in_code_block = False
    code_block_lines = []
    code_block_lang = ""

    for line in lines:
        # Handle code blocks
        if line.strip().startswith("#+begin_src"):
            in_code_block = True
            # Extract language from the line
            parts = line.strip().split()
            if len(parts) > 1:
                code_block_lang = parts[1]
            else:
                code_block_lang = ""
            code_block_lines = [f"```{code_block_lang}"]
            continue
        elif line.strip().startswith("#+end_src"):
            in_code_block = False
            code_block_lines.append("```")
            if current_section:
                current_section["content"].extend(code_block_lines)
            code_block_lines = []
            continue
        elif in_code_block:
            code_block_lines.append(line)
            continue

        # Check for main heading (single *)
        if line.strip().startswith("* ") and not line.strip().startswith("** "):
            # Save previous section if exists
            if current_section:
                sections.append(current_section)

            # Start new section
            title = line.strip()[2:].strip()  # Remove '* ' prefix
            # Convert inline code in title for display
            title_display = convert_org_to_markdown(title)
            current_section = {
                "title": title_display,
                "filename": sanitize_filename(title),  # Use original for filename
                "content": [],
            }
        elif current_section:
            # Skip org mode keywords and options
            if line.strip().startswith("#+"):
                continue

            # Convert org headings to markdown headings
            if line.strip().startswith("**"):
                # Count asterisks to determine heading level
                asterisk_count = len(line) - len(line.lstrip("*"))
                # Get the heading text and convert inline code
                heading_text = line.strip("* ").strip()
                heading_text = convert_org_to_markdown(heading_text)
                # Convert to markdown (start at ## level, so add 1 to the count)
                markdown_heading = "#" * asterisk_count + " " + heading_text
                current_section["content"].append(markdown_heading)
            else:
                # Regular content - convert basic org syntax to markdown
                converted_line = convert_org_to_markdown(line)
                current_section["content"].append(converted_line)

    # Don't forget the last section
    if current_section:
        sections.append(current_section)

    return sections


def convert_org_to_markdown(line: str) -> str:
    """Convert basic org syntax to markdown."""
    # First, protect inline code and verbatim text by temporarily replacing them
    code_blocks = []
    verbatim_blocks = []

    # Extract and protect =code= blocks
    def protect_code(match):
        code_blocks.append(match.group(1))
        return f"__CODE_BLOCK_{len(code_blocks)-1}__"

    # Extract and protect ~verbatim~ blocks
    def protect_verbatim(match):
        verbatim_blocks.append(match.group(1))
        return f"__VERBATIM_BLOCK_{len(verbatim_blocks)-1}__"

    # Protect inline code and verbatim first
    line = re.sub(r"=([^=]+)=", protect_code, line)
    line = re.sub(r"~([^~]+)~", protect_verbatim, line)

    # Bold: *text* -> **text** (but avoid conflicts with headings)
    if not line.strip().startswith("*"):
        line = re.sub(r"\*([^*\s][^*]*[^*\s])\*", r"**\1**", line)
        line = re.sub(r"\*([^*\s])\*", r"**\1**", line)  # Single character bold

    # Italic: /text/ -> *text*
    line = re.sub(r"/([^/\s][^/]*[^/\s])/", r"*\1*", line)
    line = re.sub(r"/([^/\s])/", r"*\1*", line)  # Single character italic

    # Links: [[url][text]] -> [text](url)
    line = re.sub(r"\[\[([^\]]+)\]\[([^\]]+)\]\]", r"[\2](\1)", line)

    # Simple links: [[url]] -> [url](url)
    line = re.sub(r"\[\[([^\]]+)\]\]", r"[\1](\1)", line)

    # Lists: convert org lists to markdown
    if line.strip().startswith("- "):
        pass  # Already markdown format
    elif line.strip().startswith("+ "):
        line = line.replace("+ ", "- ", 1)  # Convert + to -

    # Restore protected code blocks with backticks
    for i, code in enumerate(code_blocks):
        line = line.replace(f"__CODE_BLOCK_{i}__", f"`{code}`")

    # Restore protected verbatim blocks with backticks
    for i, verbatim in enumerate(verbatim_blocks):
        line = line.replace(f"__VERBATIM_BLOCK_{i}__", f"`{verbatim}`")

    return line


def create_starlight_frontmatter(title: str, description: str = "") -> str:
    """Create Starlight-compatible frontmatter."""
    if not description:
        description = f"Documentation for {title}"

    frontmatter = f"""---
title: {title}
description: {description}
---

"""
    return frontmatter


def write_markdown_file(section: Dict, output_dir: Path) -> None:
    """Write a section to a markdown file with Starlight frontmatter."""
    filename = f"{section['filename']}.md"
    filepath = output_dir / filename

    # Create frontmatter
    frontmatter = create_starlight_frontmatter(section["title"])

    # Combine frontmatter and content
    content = frontmatter + "\n".join(section["content"])

    # Write to file
    with open(filepath, "w", encoding="utf-8") as f:
        f.write(content)

    print(f"Created: {filepath}")


def convert_org_to_starlight(org_file: Path, output_dir: Path) -> None:
    """Convert org file to Starlight markdown files."""
    # Read org file
    with open(org_file, "r", encoding="utf-8") as f:
        content = f.read()

    # Parse content
    sections = parse_org_content(content)

    # Create output directory if it doesn't exist
    output_dir.mkdir(parents=True, exist_ok=True)

    # Write each section to a separate file
    for section in sections:
        write_markdown_file(section, output_dir)

    print(f"\nConverted {len(sections)} sections from {org_file} to {output_dir}")


def main():
    parser = argparse.ArgumentParser(
        description="Convert Org files to Starlight Markdown files"
    )
    parser.add_argument("input", help="Input org file path")
    parser.add_argument(
        "-o", "--output", default="./docs", help="Output directory (default: ./docs)"
    )

    args = parser.parse_args()

    input_file = Path(args.input)
    output_dir = Path(args.output)

    if not input_file.exists():
        print(f"Error: Input file '{input_file}' does not exist")
        return 1

    if not input_file.suffix.lower() == ".org":
        print(f"Warning: Input file '{input_file}' does not have .org extension")

    try:
        convert_org_to_starlight(input_file, output_dir)
        print(
            f"\nConversion complete! Check the '{output_dir}' directory for your Starlight markdown files."
        )
    except Exception as e:
        print(f"Error during conversion: {e}")
        return 1

    return 0


if __name__ == "__main__":
    exit(main())
