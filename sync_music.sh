#!/bin/sh

# sync_music - sync music
# first create the subset forest, then sync with --delete

mountpoint=/mnt/tmp/MUSIC

forest=$(mktemp -dt forest-XXXXXXXX)
racket forest.scm ~/music "$forest" || exit 1
sudo rsync -rtvL --delete --modify-window=1 "${forest}/" "${mountpoint}/" \
  && rm -r "$forest"
