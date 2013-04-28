#!/bin/sh
# Executed by startx (run your window manager from here)

# Run startup scripts
[ -f ~/.xprofile ] && source ~/.xprofile

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
