#!/bin/bash
prev="$(pwd)"
ls -1 | while read dir
do
	[[ ! -d "$dir" ]] && continue
	cd "$dir"
	#start git stuff
	echo $dir
	git status --short
	echo
	#end git stuff
	cd "$prev"
done
