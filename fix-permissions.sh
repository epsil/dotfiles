#!/bin/bash
for dir in "$@";
do
	cd ~; cd "$dir"
	echo "Setting directory permissions ..."
	sudo nice -n -20 find -type d -exec chmod 775 {} \;
	echo "Setting file permissions ..."
	sudo nice -n -20 find -type f -exec chmod 664 {} \;
	echo "Setting file ownership ..."
	sudo nice -n -20 find -exec chown vegard:vegard {} \;
done

# Exceptions
# sudo chown root:root /home/vegard/crontab
# sudo chmod 644 /home/vegard/crontab
# sudo chmod 777 /home/vegard/Nedlastinger/Upload
