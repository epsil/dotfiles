#!/bin/bash
set -e
if [ "$1" = "--defrag" ]
then
	rm -rf /media/Vegards\ Pod/*
	./pod.sh
elif [ "$#" -gt "0" ]
then
	cd "/media/Vegards Pod"
	for f in "$@"
	do
	        if [ -e "/home/vegard/Musikk/$f" ]
		then
                        # --modify-window must be at least 1 since
                        # FAT32 represents time with a 2-second
                        # resolution. Set it to an hour due to
                        # daylight saving regimes.
			rsync -av --progress --modify-window=4000 --delete "/home/vegard/Musikk/$f" .
		fi
	done
else
	# rm -rf "/media/Vegards Pod/Calendars"
	# rm -rf "/media/Vegards Pod/Contacts"
	# rm -rf "/media/Vegards Pod/iPod_Control"
	# rm -rf "/media/Vegards Pod/Notes"
	rsync -av --progress --modify-window=4000 --delete \
	--exclude=".rockbox" --exclude=".scrobbler.log" \
	--exclude="Playlists" --exclude="playlists" \
	--exclude="*.m3u" --exclude="*.bmark" --exclude="unsorted" \
	"/home/vegard/Musikk/" "/media/Vegards Pod"
	rsync -avz --progress --modify-window=4000 "/home/vegard/Musikk/unsorted" "/media/Vegards Pod"
	rsync -avz --quiet /media/Vegards\ Pod/*.m3u /media/Vegards\ Pod/Playlists
	rsync -avz --quiet /media/Vegards\ Pod/*.bmark /media/Vegards\ Pod/Playlists
	rsync -avz --update --progress /home/vegard/Musikk/playlists/* "/media/Vegards Pod/Playlists"
	rsync -avz --update --progress /media/Vegards\ Pod/Playlists/*.m3u /home/vegard/Musikk/playlists
	rsync -avz --progress /home/vegard/Musikk/playlists/* "/media/Vegards Pod"
	rsync -avz --progress /home/vegard/Musikk/playlists/* "/media/Vegards Pod/Playlists"
	rsync -avz --quiet "/media/Vegards Pod/.scrobbler.log" /home/vegard/scrobbler.log
fi
