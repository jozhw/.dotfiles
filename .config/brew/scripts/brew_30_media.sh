#!/usr/bin/env bash
#
# Media — audio and video: download, conversion, tagging, and playback.
#
# Add here: A/V downloaders, transcoders/converters, taggers, and CLI players.
# Still-image and PDF/PostScript tools go in brew_31_graphics.sh instead.

brew install yt-dlp    # Download video/audio from YouTube and 1000+ other sites
brew install ffmpeg    # Convert and transcode virtually any audio/video format
brew install flac      # Encode/decode FLAC audio (provides the `metaflac` tagger)
brew install sox       # Command-line audio processing and playback (provides `play`)
