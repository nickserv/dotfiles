# Nick's config files

## About
This is my castle (dotfile repo) for
[homesick](https://github.com/technicalpickles/homesick), which makes it easier for me
to store and move around my config files with a git repo. Please note that my
install script installs homesick for you, so you won't need to worry about
that. :)

## Installation
```bash
wget -q https://raw.github.com/thenickperson/castle/master/Makefile
make install
```

### Packages
The `Makefile` does not install these yet, so run this to install some of my
favorite Ruby and Node.js packages:

```bash
# before you run anything, make sure you have bundler and npm installed first
cd packages
bundle
sudo npm install -g `cat node_packages.txt`
```

## Older config files
- [My old fork of Cody Krieger's vim config files](https://github.com/thenickperson/dotvim-old)
