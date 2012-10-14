#!/bin/bash

if [! command -v wget > /dev/null]; then
	echo "Installation failed. Please install wget."
	exit 1
fi

echo "Installing homeshick..."
wget -qO- https://raw.github.com/andsens/homeshick/master/install.sh | bash

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
