#!/bin/zsh

# Bins and scripts
export PATH=$PATH:~/Bin:~/.dotfiles/scripts

# Ruby and rbenv
export PATH=$PATH:~/.rbenv/bin:~/.gem/ruby/1.9.1/bin
eval "$(rbenv init -)"

# Other variables
export EDITOR=vim
export BROWSER=firefox
