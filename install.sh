#!/bin/bash

# Initial setup
if [! command -v wget > /dev/null]; then
	echo "Installation failed. Please install wget."
	exit 1
fi
pushd $HOME

# Homeshick
echo "Installing homeshick..."
wget -qO- https://raw.github.com/andsens/homeshick/master/install.sh | bash

# Castle
echo "Installing thenickperson/castle..."
$HOME/.homeshick clone git@github.com:thenickperson/castle.git
$HOME/.homeshick symlink castle
sudo chsh --shell /bin/zsh `whoami`

# Dotvim
echo "Installing thenickperson/dotvim..."
wget -qO- https://raw.github.com/thenickperson/dotvim/master/bootstrap.sh | bash

# Conclusion
popd
echo "Open a new terminal to start your proper shell."
