#!/bin/sh
#Executed by startx (run your window manager from here)

#start important stuff
xrdb -merge ~/.Xresources #update X resources database
xsetroot -cursor_name left_ptr & #show cursor over backgrounds
#echo 100000 | sudo tee /proc/sys/fs/inotify/max_user_watches #behave, Dropbox!

#start gui daemons
nm-applet --sm-disable & #networking
xfce4-power-manager & #power
thunar --daemon & #thunar daemon
urxvtd -q -o -f & #urxvt daemon
xscreensaver -no-splash & #screen lock
sleep 300 && dropboxd & #dropbox
chromium & #browser

#fix gui java apps displaying in awesome
export _JAVA_AWT_WM_NONREPARENTING=1

#tweak skype theme
export GTK2_RC_FILES="/etc/gtk-2.0/gtkrc:$HOME/.gtkrc-2.0"

#Sessions: awesome, openbox-session, startxfce4, startlxde, gnome-session
session="awesome"
exec $session > ~/.cache/wm_out 2> ~/.cache/wm_err
