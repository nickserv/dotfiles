SHELL     = /bin/bash

update: pull symlink update_vim_plugins

uninstall:
	@echo "Uninstalling nicolasmccurdy/dotfiles..."
	rm -rf ~/.homesick/repos/dotfiles
	gem uninstall homesick
	@echo "Done. You may need to manually delete leftover symlinks."

clone:
	@echo "Cloning repository..."
	if [ ! -d "$(HOME)/.homesick/repos/dotfiles" ]; \
	then \
		homesick clone https://github.com/nicolasmccurdy/dotfiles.git; \
	fi

pull:
	@echo "Pulling repository..."
	homesick pull

symlink:
	@echo "Symlinking config files..."
	homesick symlink

set_up_repos_directory:
	@echo "Setting up ~/Repos..."
	if [ ! -d "$(HOME)/Repos" ]; \
	then \
		mkdir $(HOME)/Repos; \
	fi
	ln -s $(HOME)/.homesick/repos/dotfiles $(HOME)/Repos/dotfiles

clean:
	pushd ~/.homesick/repos/dotfiles > /dev/null
	@echo "Cleaning repository..."
	git clean -dfx
	pushd > /dev/null

install_homesick:
	@echo "Installing homesick..."
	gem install homesick

use_zsh:
	@echo "Switching shell to zsh..."
	if [[ "$(shell uname)" == "Darwin" ]]; \
	then \
		chsh -s /bin/zsh; \
	else \
		sudo chsh --shell /bin/zsh `whoami`; \
	fi

install_vundle:
	@echo "Installing vundle..."
	if [ ! -d "$(HOME)/.vim/bundle" ]; \
	then \
		mkdir $(HOME)/.vim/bundle; \
	fi
	if [ ! -d "$(HOME)/.vim/bundle/Vundle.vim" ]; \
	then \
		git clone https://github.com/VundleVim/Vundle.vim.git $(HOME)/.vim/bundle/Vundle.vim; \
	fi

install_vim_plugins:
	@echo "Installing vim plugins..."
	vim +PluginInstall +qall

update_vim_plugins:
	@echo "Updating vim plugins..."
	vim +PluginInstall! +qall

install: install_homesick clone use_zsh symlink set_up_repos_directory install_vundle install_vim_plugins
	@echo "Open a new terminal to start your proper shell."
