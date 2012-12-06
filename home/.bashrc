#!/bin/bash

# Load aliases
if [ -f ~/.aliases ]; then
	. ~/.aliases
fi

# Prompt
PS1='\[\e[32m\]\u:\w\$\[\e[m\] ' # old config from crunchbang
#PS1='[\u@\h \W]\$ ' # arch default
#PS1='\[\e[32m\]\u:\w\$\[\e[m\] ' # old config from crunchbang
#PS1='\u@\h:\w$ ' # default from crunchbang

# Variables
export EDITOR='vim'
