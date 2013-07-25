SHELL     = /bin/bash
HOMESHICK = $(HOME)/.homesick/repos/homeshick/home/.homeshick

update: pull symlink update_vim_plugins

uninstall:
	@echo "Uninstalling thenickperson/castle..."
	rm -rf ~/.homesick
	rm ~/.homeshick
	@echo "Done. You may need to manually delete leftover symlinks."

clone:
	@echo "Cloning repository..."
	if [ ! -d "$(HOME)/.homesick/repos/castle" ]; \
	then \
		$(HOMESHICK) clone https://github.com/thenickperson/castle.git; \
	fi

pull:
	@echo "Pulling repository..."
	$(HOMESHICK) pull castle

symlink:
	@echo "Symlinking config files..."
	$(HOMESHICK) symlink castle

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

install_homeshick:
	@echo "Installing homeshick..."
	if [ ! -d "$(HOME)/.homesick/repos/homeshick" ]; \
	then \
		git clone git://github.com/andsens/homeshick.git $(HOME)/.homesick/repos/homeshick; \
	fi

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

install: install_homeshick clone use_zsh symlink set_up_repos_directory install_vundle install_vim_plugins
	@echo "Open a new terminal to start your proper shell."
