#!/bin/sh

# vorbiscomment wrapper

(grep -v '^CDDB='
 printf "%s" "ENCODE_DATE="
 date "+%Y-%m-%d %H:%M"
 printf "%s" "ENCODE_USER="
 id -un) \
  | vorbiscomment "$@"
