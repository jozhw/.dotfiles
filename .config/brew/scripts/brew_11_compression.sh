#!/usr/bin/env bash
#
# Compression — archivers and (de)compression utilities.
#
# Add here: archive formats and compressors/decompressors (7-Zip, gzip/zstd
# variants, tar helpers, etc.).

brew install p7zip     # 7-Zip archiver with very high compression ratios
brew install pigz      # Parallel gzip that uses all CPU cores
brew install zopfli    # Slow but very strong gzip/zlib compressor (great for web assets)
