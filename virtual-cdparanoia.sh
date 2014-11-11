#!/bin/sh

# When we receive a request for a CD track, we simply copy the
# appropriate audio file to the output destination.

# virtual cd dir
drive=~/etc/cdrom

track=$1
output=$2

cp -v "${drive}/${track}.wav" "$output"
