#!/bin/bash

echo "Installing homeshick..."
if command -v curl > /dev/null
	curl -so ~/.homeshick https://raw.github.com/andsens/homeshick/master/homeshick
elseif command -v wget > /dev/null
	wget -q https://raw.github.com/andsens/homeshick/master/homeshick
	mv homeshick ~/.homeshick
else
	echo "Please install curl or wget."
fi

echo "Installing thenickperson/castle..."
homeshick clone git@github.com:thenickperson/castle.git
homeshick symlink thenickperson/castle

echo "Installing thenickperson/dotvim..."
if command -v curl > /dev/null
	curl https://raw.github.com/thenickperson/dotvim/master/bootstrap.sh -so - | sh
elseif command -v wget > /dev/null
	wget -q https://raw.github.com/thenickperson/dotvim/master/bootstrap.sh
	bash bootstrap.sh
else
	echo "Please install curl or wget."
fi
