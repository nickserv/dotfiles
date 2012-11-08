#!/bin/zsh

# Load OS scripts/software
source /etc/profile

# Bins and scripts
if [[ $EUID -ne 0 ]]; then
	export PATH=$PATH:~/Bin:~/Repos/castle/scripts
fi

# Load additional software
# Heroku
export PATH=$PATH:/usr/local/heroku/bin
# RubyGems
export PATH=$PATH:~/.gem/ruby/1.9.1/bin
# rbenv
#export PATH=$PATH:~/.rbenv/bin
#eval "$(rbenv init -)"

# Other variables
export EDITOR=vim
export BROWSER=chromium
