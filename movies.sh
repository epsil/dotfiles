#!/bin/bash
set -e
cd "/home/vegard/Videoklipp/filmer"
for f in *
do
	# rsync -av --progress --delete --dry-run "$f" "/media/Filmer/$f"
	rsync -av --progress --delete "$f" "/media/Filmer"
done
