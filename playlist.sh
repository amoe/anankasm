#!/bin/sh

# /usr/lib/anankasm/playlist.sh
# or "anankasm playlist"

# album-based m3u playlist generator

(for path; do
    find "$path" -type f | sort
done) \
  | sed -e 's|^|/|'
