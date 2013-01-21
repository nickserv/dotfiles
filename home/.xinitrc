#!/bin/sh
# Executed by startx (run your window manager from here)

# Start important X stuff
xrdb -merge ~/.Xresources        #update X resources database
xsetroot -cursor_name left_ptr & #show cursor over backgrounds
#echo 100000 | sudo tee /proc/sys/fs/inotify/max_user_watches #behave, Dropbox!

# Tweak trackpad settings
synclient VertTwoFingerScroll=1
synclient HorizTwoFingerScroll=1
synclient EmulateTwoFingerMinW=5
synclient EmulateTwoFingerMinZ=48

# Fix Java GUIs
export _JAVA_AWT_WM_NONREPARENTING=1

# Fix Skype theme
export GTK2_RC_FILES="/etc/gtk-2.0/gtkrc:$HOME/.gtkrc-2.0"

# Startup items
nm-applet --sm-disable &  # networking
urxvtd -q -o -f &         # urxvt daemon
dropboxd &                # dropbox
google-chrome &           # web browser

# Choose a graphical session
# Examples: awesome, openbox-session, startxfce4, startlxde, gnome-session
session="dwm"

# Configs for dwm
if [[ "$session" == "dwm" ]]; then
	feh --bg-fill ~/wallpaper.jpg
	while true; do
		BAT="$(acpi -b | awk 'sub(/,/,"") {print $3, $4}')"
		TIME="$(date +"%a, %b %d | %I:%M %p")"
		xsetroot -name "$BAT | $TIME"

		sleep 1m
	done &
fi

# Start WM/DE
exec $session > ~/.cache/wm_out 2> ~/.cache/wm_err
