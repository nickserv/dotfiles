###############
# Completions #
###############

# Completions
autoload -U compinit
compinit -C

# Arrow key menu for completions
zstyle ':completion:*' menu select

# Case-insensitive (all),partial-word and then substring completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# Disable spell correction (too many false positives)
unsetopt correct_all

###########
# Aliases #
###########

# Set up aliases
. ~/.aliases

# Autocomplete command line switches for aliases
setopt completealiases

###########
# History #
###########

# number of lines kept in history
export HISTSIZE=1000
# number of lines saved in the history after logout
export SAVEHIST=1000
# location of history
export HISTFILE=~/.zsh_history
# append command to history file once executed
setopt inc_append_history

##########
# Prompt #
##########

# Function that displays the hostname if the current session is over SSH
function ssh_info() {
  if [[ -n $SSH_CONNECTION ]]; then
    echo "%{$fg[red]%}$(hostname -s) "
  fi
}

function() dir_info() {
  # if normal user
  if [[ $EUID -ne 0 ]]; then
   echo "%{$fg[cyan]%}%~"
  else
   echo "%{$fg[red]%}%~"
  fi
}

# Prompt
autoload -U colors && colors
setopt prompt_subst
# Colors: black red green yellow blue magenta cyan white
export PROMPT='%{$(title_info)%}$(ssh_info)$(dir_info) %{$fg[yellow]%}→ %{$reset_color%}'

#############
# Title Bar #
#############

# Set up the title bar text
title_info() {
  if [[ $TERM == xterm* ]]; then
    print -Pn "\e]2;%~\a"
  fi
}

# Enable title bar info display
# This gets called every time the working directory changes.
chpwd() {
  [[ -t 1 ]] || return
  title_info
}
# This extra call is here so that the title bar will also be set when a new
# terminal is created.
title_info

########
# Misc #
########

# Automatically use cd when paths are entered without cd
setopt autocd

# Autojump
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

# nvm
. "$(brew --prefix nvm)/nvm.sh"

############
# Keybinds #
############

# Use emacs keybinds, since they're modeless and closer to the bash defaults
bindkey -e

# When going through the command history, only show commands that include the
# current input
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward
bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward

eval "$(rbenv init -)"
