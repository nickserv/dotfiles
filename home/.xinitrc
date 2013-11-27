#!/bin/sh
# Executed by startx (run your window manager from here)

# Run startup scripts
[ -f ~/.xprofile ] && source ~/.xprofile

# Choose a graphical session
# Examples: awesome, openbox-session, startxfce4, startlxde, gnome-session
session="xmonad"

# Configs for dwm
if [[ "$session" == "dwm" ]]; then
  while true; do
    BAT="$(acpi -b | awk '{ print $4 " " $5 }' | tr -d ',')"
    TIME="$(date +"%a, %b %d | %I:%M %p")"
    xsetroot -name "$BAT | $TIME"

    sleep 1m
  done &
fi

if [[ "$session" == "dwm" || "$session" == "xmonad" ]]; then
  feh --bg-fill ~/wallpaper.jpg
fi

# Start WM/DE
exec $session > ~/.cache/wm_out 2> ~/.cache/wm_err
