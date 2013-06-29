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
    RAM="$(free -m | awk '/cache:/ {print $3"M" }')"
    TIME="$(date +"%a, %b %d | %I:%M %p")"
    xsetroot -name "$RAM | $BAT | $TIME"

    sleep 1m
  done &
fi

# Start WM/DE
exec $session > ~/.cache/wm_out 2> ~/.cache/wm_err
