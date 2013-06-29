#!/bin/zsh

# Load OS scripts/software
source /etc/profile

# Load additional software
# RubyGems
export GEM_HOME=~/.gem/ruby/2.0.0
# rbenv
#export PATH=$PATH:~/.rbenv/bin
#eval "$(rbenv init - --no-rehash)"

# Other variables
export EDITOR=vim
export BROWSER=firefox
export RAILS_ENV=development

# Tell SSH servers that we're using a vt100-compatible terminal
# (prevents annoying compatibility issues)
if [[ -n $SSH_CONNECTION ]]; then
  export TERM=vt100
fi
