#!/bin/bash

dir=~/Musikk
# file=${1:-~/playlists/todo+new.m3u}

while getopts "d:" OPTION
do
        case $OPTION in
                d)
                        dir=$OPTARG
                        ;;
        esac
done

shift $(($OPTIND - 1))

for file in "$@"
do
        echo $file
        while read line
        do
	        if ( cd "$dir" && [ ! -e "$line" ] )
                then
	                echo -e "    $line"
                fi
        done < "$file"
        echo
done
