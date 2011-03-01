#!/bin/sh

# sync_music - sync music
# first create the subset forest, then sync with --delete

forest=$(mktemp -dt forest-XXXXXXXX)
mzscheme ~/ripsys/forest.scm ~/music "$forest" || exit 1
rsync -rtvL --delete --modify-window=1 "${forest}/" "/media/Sansa e280/music" \
  && rm -r "$forest"
