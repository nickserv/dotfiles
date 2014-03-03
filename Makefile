SHELL     = /bin/bash

update: pull symlink update_vim_plugins

uninstall:
	@echo "Uninstalling nicolasmccurdy/castle..."
	rm -rf ~/.homesick/repos/castle
	gem uninstall homesick
	@echo "Done. You may need to manually delete leftover symlinks."

clone:
	@echo "Cloning repository..."
	if [ ! -d "$(HOME)/.homesick/repos/castle" ]; \
	then \
		homesick clone https://github.com/nicolasmccurdy/castle.git; \
	fi

pull:
	@echo "Pulling repository..."
	homesick pull castle

symlink:
	@echo "Symlinking config files..."
	homesick symlink castle

set_up_repos_directory:
	@echo "Setting up ~/Repos..."
	if [ ! -d "$(HOME)/Repos" ]; \
	then \
		mkdir $(HOME)/Repos; \
	fi
	ln -s $(HOME)/.homesick/repos/castle $(HOME)/Repos/castle

clean:
	pushd ~/.homesick/repos/castle > /dev/null
	@echo "Cleaning repository..."
	git clean -dfx
	pushd > /dev/null

install_homesick:
	@echo "Installing homesick..."
	gem install homesick

use_zsh:
	@echo "Switching shell to zsh..."
	sudo chsh --shell /bin/zsh `whoami`

install_vundle:
	@echo "Installing vundle..."
	if [ ! -d "$(HOME)/.vim/bundle" ]; \
	then \
		mkdir $(HOME)/.vim/bundle; \
	fi
	if [ ! -d "$(HOME)/.vim/bundle/vundle" ]; \
	then \
		git clone https://github.com/gmarik/vundle.git $(HOME)/.vim/bundle/vundle; \
	fi

install_vim_plugins:
	@echo "Installing vim plugins..."
	vim +BundleInstall +qall

update_vim_plugins:
	@echo "Updating vim plugins..."
	vim +BundleInstall! +qall

install: install_homesick clone use_zsh symlink set_up_repos_directory install_vundle install_vim_plugins
	@echo "Open a new terminal to start your proper shell."
