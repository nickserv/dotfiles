#!/bin/zsh

# Load OS scripts/software
source /etc/profile

# Set path
path=(
  $path
  /usr/local/sbin
  $(brew --prefix)/opt/ruby/bin
  $GEM_HOME/bin
  $HOME/Library/Haskell/bin
  $HOME/Repos/dotfiles/bin
)

# Load additional software
# RubyGems
export GEM_HOME=~/.gem/ruby/2.0.0

# Other variables
export EDITOR='atom --wait'
export ANT_ARGS='-logger org.apache.tools.ant.listener.AnsiColorLogger'
export LLVM=/usr/local/opt/emscripten/libexec/llvm/bin
