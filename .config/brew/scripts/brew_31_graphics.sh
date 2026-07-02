#!/usr/bin/env bash
#
# Graphics — still images and documents (PDF/PostScript).
#
# Add here: raster/vector image manipulation and conversion, plus document
# rendering/tooling (PDF, PostScript). Audio/video belongs in brew_30_media.sh.

brew install gs                            # Ghostscript — PostScript and PDF interpreter/converter
brew install imagemagick                   # Convert, resize, and edit images from the command line
brew install poppler automake pkg-config   # PDF rendering/tools (poppler) plus its build deps
#brew install exiv2                         # Read/write image (EXIF/IPTC) metadata (disabled)
