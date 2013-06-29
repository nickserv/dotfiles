#!/bin/bash

# Load aliases
if [ -f ~/.aliases ]; then
  . ~/.aliases
fi

# Prompt
PS1='\[\e[0;34m\]\W\[\e[m\] \[\e[0;33m\]\$\[\e[m\] '

# Variables
export EDITOR='vim'
