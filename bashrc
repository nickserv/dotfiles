#!/bin/bash

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# prompts
PS1="\[\e[32m\]\u:\w\$\[\e[m\] " #old config from crunchbang
#PS1="[\u@\h \W]\$ " #arch default
#PS1="\[\e[32m\]\u:\w\$\[\e[m\] " #old config from crunchbang
#PS1="\u@\h:\w$ " #default from crunchbang

# variables
export EDITOR="vim"

# bash aliases
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi
