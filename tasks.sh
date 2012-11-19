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

check_wget () {
	if [! command -v wget > /dev/null]; then
		echo "Installation failed. Please install wget."
		popd > /dev/null
		exit 1
	fi
}

uninstall () {
	echo "Uninstalling thenickperson/castle..."
	rm -r ~/.homesick
	rm ~/.homeshick
	echo "Done. You may need to manually delete leftover symlinks."
}

clone () {
	echo "Cloning repository..."
	$HOME/.homeshick clone https://github.com/thenickperson/castle.git
}

pull () {
	echo "Pulling repository..."
	$HOME/.homeshick pull castle
}

symlink () {
	echo "Symlinking config files..."
	$HOME/.homeshick symlink castle
}

set_up_repos_directory () {
	echo "Setting up ~/Repos..."
	if [ ! -d "$HOME/Repos" ]; then
		mkdir $HOME/Repos
	fi
	ln -s $HOME/.homesick/repos/castle $HOME/Repos/castle
}

clean () {
	pushd ~/.homesick/repos/castle > /dev/null
	echo "Cleaning repository..."
	git clean -dfx
	pushd > /dev/null
}

install_homeshick () {
	echo "Installing homeshick..."
	wget -qO- https://raw.github.com/andsens/homeshick/master/install.sh | bash
}

use_zsh () {
	echo "Switching shell to zsh..."
	sudo chsh --shell /bin/zsh `whoami`
}

install_vundle () {
	echo "Installing vundle..."
	if [ ! -d "$HOME/.vim/bundle" ]; then
		mkdir $HOME/.vim/bundle
	fi
	if [ ! -d "$HOME/.vim/bundle/vundle" ]; then
		git clone https://github.com/gmarik/vundle.git $HOME/.vim/bundle/vundle
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

install () {
	echo "Installing thenickperson/castle..."
	check_wget
	install_homeshick
	clone
	use_zsh
	symlink
	set_up_repos_directory
	install_vundle
	install_vim_plugins
	echo "Open a new terminal to start your proper shell."
}

pushd $HOME > /dev/null
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
popd > /dev/null
