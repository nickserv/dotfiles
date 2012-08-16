# Default Mod by [Nicolas McCurdy](http://thenickperson.com)
A slightly improved version of awesome's default theme. Default Mod aims to be
a better template for starting new awesome themes.

## Changes from the original default theme

Please note that the updated floating window icons are currently the only
visible modifications made in this theme.

### Theme Management

- a single variable called themedir controls where all theme resources are
	loaded from (for easy moving and renaming of themes)

- the readme is formatted in markdown, making it ready for display on GitHub
	(you're reading it right now!)

### Icons

- those weird and confusing wing icons for representing floating windows are
	replaced with simple white and black "F" icons

- awesome's default icons are included in their own folder called "icons" (for
	easy including of modified icons in new themes)

- a white version of the submenu icon is included

- submenu icons are in their own folder called "menu"

## Installation

1. Download the theme and move (or symlink) it to ~/.config/awesome/themes/.
If you want to put it somewhere else, you will need to update the value of
themedir (in theme.lua) to the proper location.

2. Edit your rc.lua file and change your beautiful theme so there's a line that
looks like this:
`beautiful.init("/home/USERNAME/.config/awesome/themes/awesomebang/theme.lua")`

3. Restart awesome (super-ctrl-r) and enjoy!

## License

- [CC BY-NC 3.0](http://creativecommons.org/licenses/by-nc/3.0/)

- Based off of the default awesome theme by awesome's developers.

- Background images from the default awesome theme by Mikael Eriksson
<mikael_eriksson@miffe.org>, licensed under CC BY-SA-3.0.
