#!/bin/bash

# Simple Rakefile-esque task runner.
# Call it like this:
#
#   ./tasks.sh pull symlink
#
# or just:
#
#   ./tasks.sh
#
# which will execute the pull, symlink, and update_vim_plugins tasks.

install_vundle () {
	echo "Installing vundle..."
	if [ ! -d "home/.vim/bundle" ]; then
		mkdir home/.vim/bundle
	fi
	if [ ! -d "home/.vim/bundle/vundle" ]; then
		git clone git://github.com/gmarik/vundle.git home/.vim/bundle/vundle
	fi
}

install_vim_plugins () {
	echo "Installing vim plugins..."
	vim +BundleInstall +qall
}

update_vim_plugins () {
	echo "Updating vim plugins..."
	vim +BundleInstall! +qall
}

clean () {
	git clean -dfx
}

symlink () {
	echo "Symlinking config files..."
	homesick symlink castle
}

pull () {
	echo "Pulling latest from $(git config remote.origin.url)..."
	git pull
}

install () {
	echo "Installing config files..."
	pull
	symlink
	install_vundle
	install_vim_plugins
}

if [ $# -gt 0 ]
then
	for i in $@
	do
		[ "`type -t $i`" == "function" ] && $i || echo "No task named $i."
	done
else
	pull
	symlink
	update_vim_plugins
fi
