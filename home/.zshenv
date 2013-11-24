#!/bin/zsh

# Load OS scripts/software
source /etc/profile

# Load additional software
# rbenv
#export PATH=$PATH:~/.rbenv/bin
#eval "$(rbenv init - --no-rehash)"
# RubyGems
export GEM_HOME=~/.gem/ruby/2.0.0

# Other variables
export BROWSER=firefox
export EDITOR=vim
export RAILS_ENV=development
export ANT_ARGS='-logger org.apache.tools.ant.listener.AnsiColorLogger'
export TERM=rxvt
