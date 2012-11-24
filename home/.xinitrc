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

# Launch startup applications
nm-applet --sm-disable &  #networking
xfce4-power-manager &     #power
thunar --daemon &         #thunar daemon
urxvtd -q -o -f &         #urxvt daemon
xscreensaver -no-splash & #screen lock
sleep 300 && dropboxd &   #dropbox
google-chrome &           #web browser

# Fix Java GUIs
export _JAVA_AWT_WM_NONREPARENTING=1

# Fix Skype theme
export GTK2_RC_FILES="/etc/gtk-2.0/gtkrc:$HOME/.gtkrc-2.0"

# Start a graphical session
# Examples: awesome, openbox-session, startxfce4, startlxde, gnome-session
session="awesome"
exec $session > ~/.cache/wm_out 2> ~/.cache/wm_err
