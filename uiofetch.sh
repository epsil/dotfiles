#!/bin/sh
# Script for syncing FROM UiO TO the hard drive.
# Symlink to /usr/local/bin.
for dir in "$@";
do
	rsync -avz --progress vegardoy@login.ifi.uio.no:$dir ~/Dokumenter
done
