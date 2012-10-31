#!/bin/zsh

# Bins and scripts
if [[ $EUID -ne 0 ]]; then
	export PATH=$PATH:~/Bin:~/Repos/castle/scripts
fi

# Ruby and rbenv
export PATH=$PATH:~/.rbenv/bin:~/.gem/ruby/1.9.1/bin
eval "$(rbenv init -)"

# Other variables
export EDITOR=vim
export BROWSER=chromium

skip_global_compinit=1
export PATH=$PATH:/usr/local/heroku/bin
