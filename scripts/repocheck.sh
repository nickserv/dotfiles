#!/bin/bash
CWD="$(pwd)" #save starting directory
ls -1 | while read d
do
	[[ ! -d "$d" ]] && continue
	cd "$d"
	echo "Changed to directory" $d
	#Now you can do someting in the sub-directory
	cd "$CWD" #Change back to old directory
done
