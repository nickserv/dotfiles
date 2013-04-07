#!/bin/sh
# Executed by startx (run your window manager from here)

# Start important X stuff
xrdb -merge ~/.Xresources        #update X resources database
xsetroot -cursor_name left_ptr & #show cursor over backgrounds

# Tweak trackpad settings
synclient VertTwoFingerScroll=1
synclient HorizTwoFingerScroll=1
synclient EmulateTwoFingerMinW=5
synclient EmulateTwoFingerMinZ=48

# Fix Java GUIs
export _JAVA_AWT_WM_NONREPARENTING=1

# Fix Skype theme
export GTK2_RC_FILES="/etc/gtk-2.0/gtkrc:$HOME/.gtkrc-2.0"

# Use the GTK theme for Java Swing applications
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on
-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"

# Startup items
nm-applet --sm-disable &           # networking
urxvtd -q -o -f &                  # urxvt daemon
dropboxd &                         # dropbox
google-chrome &                    # web browser
xautolock -time 10 -locker slock & # screen locker
wmname LG3D &                      # fix Java GUIs on non-reparenting WMs
udiskie &                          # automount volumes

# Choose a graphical session
# Examples: awesome, openbox-session, startxfce4, startlxde, gnome-session
session="dwm"

# Configs for dwm
if [[ "$session" == "dwm" ]]; then
	feh --bg-fill ~/wallpaper.png
	while true; do
		BAT="$(acpi -b | awk '{ print $4 " " $5 }' | tr -d ',')"
		VOL="$(amixer get Master | tail -1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')"
		TIME="$(date +"%a, %b %d | %I:%M %p")"
		TEMP="$(($(cat /sys/class/thermal/thermal_zone0/temp) / 1000))C"
		xsetroot -name "$TEMP | $BAT | $VOL | $TIME"

		sleep 1m
	done &
fi

# Start WM/DE
exec $session > ~/.cache/wm_out 2> ~/.cache/wm_err
