#!/bin/bash

echo "Installing homeshick..."
if command -v curl > /dev/null; then
	curl -so ~/homeshick https://raw.github.com/andsens/homeshick/master/homeshick
elif command -v wget > /dev/null; then
	wget -qO ~/homeshick https://raw.github.com/andsens/homeshick/master/homeshick
else
	echo "Homeshick installation failed. Please install curl or wget."
	exit 1
fi
chmod a+x ~/homeshick

echo "Installing thenickperson/castle..."
~/homeshick clone git@github.com:thenickperson/castle.git
~/homeshick symlink thenickperson/castle

echo "Installing thenickperson/dotvim..."
if command -v curl > /dev/null; then
	curl https://raw.github.com/thenickperson/dotvim/master/bootstrap.sh -so - | sh
elif command -v wget > /dev/null; then
	wget -q https://raw.github.com/thenickperson/dotvim/master/bootstrap.sh
	bash bootstrap.sh
fi
