#!/bin/bash
cd /home/vegard
if [ "$1" = "--quick" ]
then
	echo "Skipping file permissions ..."
else
	./fix-permissions.sh /home/vegard/Musikk
fi
cd /home/vegard
if [ -e "/home/vegard/Musikk/iTunes" ]; then
	read -p "Delete useless iTunes folder? (y/n) "
	if [ "$REPLY" == "y" ]; then
		rm -rf /home/vegard/Musikk/iTunes
	fi
fi
read -p "Update MPD? (y/n) "
if [ "$REPLY" == "y" ]; then
	mpc --wait --verbose update
fi
if [ -e "/media/Vegards Pod" ]; then
	read -p "Update iPod? (y/n) "
	if [ "$REPLY" == "y" ]; then
		./pod.sh
	fi
fi
if [ -e "/media/Musikk" ]; then
	read -p "Back up music? (y/n) "
	if [ "$REPLY" == "y" ]; then
		rsync -av --delete --progress /home/vegard/Musikk/ /media/Musikk
	fi
fi
