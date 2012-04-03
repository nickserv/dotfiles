# Is oh-my-zsh installed?
if [ -d ~/.oh-my-zsh ]; then #this computer is awesome
	# Path to your oh-my-zsh configuration
	ZSH=$HOME/.oh-my-zsh
	# Set name of the theme to load from ~/.oh-my-zsh/themes/
	ZSH_THEME="simplyblue"
	# Display red dots while waiting for completion
	COMPLETION_WAITING_DOTS="true"
	# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
	# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
	plugins=(ant archlinux gem git lol mercurial nyan python rails rails3 rake ruby rvm svn)
	# Run oh my zsh
	source $ZSH/oh-my-zsh.sh
else #this computer sucks
	echo "You don't have oh-my-zsh. You suck. Get it!"
fi

#exec 2>>( while read X; do print "\e[91m${X}\e[0m" > /dev/tty; done & )
export LD_PRELOAD="/home/nicolas/Bin/stderred/lib64/stderred.so"

# variables
export EDITOR="vim"
export TERM="xterm"

# aliases
if [ -f ~/.aliases ]; then
	. ~/.aliases
fi

# colored man pages (from https://wiki.archlinux.org/index.php/Man_Page#Colored_man_pages)
man() {
	env \
		LESS_TERMCAP_mb=$(printf "\e[1;31m") \
		LESS_TERMCAP_md=$(printf "\e[1;31m") \
		LESS_TERMCAP_me=$(printf "\e[0m") \
		LESS_TERMCAP_se=$(printf "\e[0m") \
		LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
		LESS_TERMCAP_ue=$(printf "\e[0m") \
		LESS_TERMCAP_us=$(printf "\e[1;32m") \
			man "$@"
}
