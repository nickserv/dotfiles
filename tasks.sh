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

start_install() {
	if [! command -v wget > /dev/null]; then
		echo "Installation failed. Please install wget."
		popd > /dev/null
		exit 1
	fi
	echo "Installing thenickperson/castle..."
}

end_install() {
	echo "Open a new terminal to start your proper shell."
}

uninstall() {
	echo "Uninstalling thenickperson/castle..."
	rm -r ~/.homesick
	rm ~/.homeshick
	echo "Done. You may need to manually delete leftover symlinks."
}

clone_repo() {
	echo "Cloning repository..."
	$HOME/.homeshick clone git@github.com:thenickperson/castle.git
}

install_homeshick() {
	echo "Installing homeshick..."
	wget -qO- https://raw.github.com/andsens/homeshick/master/install.sh | bash
}

use_zsh() {
	echo "Switching shell to zsh..."
	sudo chsh --shell /bin/zsh `whoami`
}

install_vundle () {
	echo "Installing vundle..."
	if [ ! -d "$HOME/.vim/bundle" ]; then
		mkdir $HOME/.vim/bundle
	fi
	if [ ! -d "$HOME/.vim/bundle/vundle" ]; then
		git clone git://github.com/gmarik/vundle.git $HOME/.vim/bundle/vundle
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
	pushd ~/.homesick/repos/castle > /dev/null
	echo "Cleaning repository..."
	git clean -dfx
	pushd > /dev/null
}

symlink () {
	echo "Symlinking config files..."
	$HOME/.homeshick symlink castle
}

pull () {
	echo "Pulling repository..."
	$HOME/.homeshick pull castle
}

install () {
	echo "Installing config files..."
	start_install
	install_homeshick
	clone_repo
	use_zsh
	symlink
	install_vundle
	install_vim_plugins
	end_install
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
