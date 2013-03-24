#!/bin/zsh

# Load OS scripts/software
source /etc/profile

# Bins and scripts
if [[ $EUID -ne 0 ]]; then
	export PATH=$PATH:~/bin:~/Repos/castle/scripts
fi

# Load additional software
# Heroku
export PATH=$PATH:/usr/local/heroku/bin
# RubyGems
export PATH=$PATH:~/.gem/ruby/2.0.0/bin
# rbenv
#export PATH=$PATH:~/.rbenv/bin
#eval "$(rbenv init - --no-rehash)"

# Other variables
export EDITOR=vim
export BROWSER=google-chrome

# Tell SSH servers that we're using a vt100-compatible terminal
# (prevents annoying compatibility issues)
if [[ -n $SSH_CONNECTION ]]; then
	export TERM=vt100
fi
