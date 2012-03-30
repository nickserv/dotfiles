#!/bin/bash

cd ~
git clone https://github.com/thenickperson/configs.git ~/.ghconfigs
selected_configs=list of all files NOT in {.*,README.md,install.sh,*_examples}
for item in $selected_configs
do
	if -a "~./"+$item_name
		mv "."+item_name "."+item_name+".old"
		echo "backup .$item_name.old"
	fi
	ln -s repo_dir/item_name ~/.item_name
	echo "~/."+item_name+" created"
done
echo "installation complete"

#TODO
#	if on a Mac, blacklist configs for Linux only software
# more *_base files instead of linking them
