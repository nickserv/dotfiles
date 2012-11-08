#!/bin/sh
#~/.xinitrc
#Executed by startx (run your window manager from here)

#what the hell does this do?
#if [ -d /etc/X11/xinit/xinitrc.d ]; then
  #for f in /etc/X11/xinit/xinitrc.d/*; do
    #[ -x "$f" ] && . "$f"
  #done
  #unset f
#fi

#test for an existing bus daemon, just to be safe
#if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
    ##if not found, launch a new one
    #eval `dbus-launch --sh-syntax --exit-with-session`
    #echo "D-Bus per-session daemon address is: $DBUS_SESSION_BUS_ADDRESS"
#fi

#start important stuff
xrdb -merge ~/.Xresources #merge X resources
xsetroot -cursor_name left_ptr & #show cursor over backgrounds
#echo 100000 | sudo tee /proc/sys/fs/inotify/max_user_watches #behave, Dropbox!

#start gui daemons
nm-applet --sm-disable & #networking
xfce4-power-manager & #power
thunar --daemon & #thunar daemon
urxvtd -q -o -f & #urxvt daemon
xscreensaver -no-splash & #screen lock
sleep 300 && dropboxd & #dropbox
chromium &

#fix gui java apps displaying in awesome
_JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_AWT_WM_NONREPARENTING

#tweak skype theme
export GTK2_RC_FILES="/etc/gtk-2.0/gtkrc:$HOME/.gtkrc-2.0"

#WMs: awesome, startxfce4
wm="awesome"
#wm="openbox-session"
#wm="startlxde"
#wm="gnome-session"
exec $wm > ~/.cache/wm_out 2> ~/.cache/wm_err
