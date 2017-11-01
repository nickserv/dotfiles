# Nick's config files

## About
This is my config file repo for
[Stow](https://www.gnu.org/software/stow/), which makes it easier for me
to store and move around my config files with a git repo. Please note
that you must install Stow yourself before running the install script.

## Installation
```bash
wget -q https://raw.github.com/nickmccurdy/dotfiles/master/install
./install
```

### Packages
The install script does not install these yet, so run this to install some of my
favorite Ruby and Node.js packages:

```bash
# before you run anything, make sure you have bundler and npm installed first
cd packages
bundle
npm install -g `cat node_packages.txt`
```

## Older config files
- [Fork of Cody Krieger's vim config](https://github.com/nickmccurdy/dotvim-old)
- [Fork of dwm](https://github.com/nickmccurdy/dwm)
