#!/bin/bash

cd ~
git clone https://github.com/thenickperson/configs.git ~/.ghconfigs
selected_configs=list of all files NOT in {.*,README.md,install.sh,*_examples}
#for i in ~/.vim ~/.vimrc ~/.gvimrc; do [ -e $i ] && mv $i $i.old; done
#git clone git://github.com/codykrieger/dotvim.git ~/.vim
#cd ~/.vim
#rake install
for item in $(ls -a | grep)
do
	if -a "~./"+$item_name
		mv .$item_name .$item_name.old
		echo "backup .$item_name.old"
	fi
	ln -s ~/.ghconfigs/$item_name ~/.$item_name
	echo "~/."+item_name+" created"
done
echo "installation complete"

#TODO
#	if on a Mac, blacklist configs for Linux only software
# more *_base files instead of linking them
