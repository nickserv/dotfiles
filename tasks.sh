#!/bin/bash

# Simple Rakefile-esque task runner.
# Call it like this:
#
#   ./tasks.sh pull update_plugins link_vimrc
#
# or just:
#
#   ./tasks.sh
#
# which will execute the pull and update_plugins tasks.

install_plugins () {
	echo "Installing plugins..."
	vim +BundleInstall +qall
}

update_plugins () {
	echo "Updating plugins..."
	vim +BundleInstall! +qall
}

clean () {
	git clean -dfx
}

link_vimrc () {
	echo "Linking ~/.vimrc..."
	ln -is $PWD/vimrc ~/.vimrc

	#echo "Linking ~/.vimrc and ~/.gvimrc..."
	#ln -is $PWD/vimrc ~/.vimrc
	#ln -is $PWD/gvimrc ~/.gvimrc
}

pull () {
	echo "Pulling latest from $(git config remote.origin.url)..."
	git pull
}

install () {
	echo "Installing VIM bootstrap of awesomesauce..."
	pull
	install_plugins
	link_vimrc
}

if [ $# -gt 0 ]
then
	for i in $@
	do
		[ "`type -t $i`" == "function" ] && $i || echo "No task named $i."
	done
else
	pull
	update_plugins
fi
