#!/bin/bash

sysresources=/etc/X11/xinit/.Xresources
userresources=$HOME/.Xresources

[ -f "$sysresources" ] && xrdb -merge "$sysresources"
[ -f "$userresources" ] && xrdb -merge "$userresources"

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# display
xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal
feh --bg-scale ~/Documents/Pictures/Wallpapers/dolomites.jpg

# cursor
xrdb -merge ~/.Xresources &&

# panel
~/.config/polybar/launch.sh

# XDG autostart

# cherry-pick autostats from system
dex /etc/xdg/autostart/blueman.desktop
dex /etc/xdg/autostart/nm-applet.desktop
dex /etc/xdg/autostart/print-applet.desktop
dex /usr/share/applications/redshift-gtk.desktop
dex /usr/share/applications/polkit-gnome-authentication-agent-1.desktop

# autostart user apps
dex -a -s ~/.config/autostart

# TODO do we need this?
# - gnome-keyring

# processes strated by systemd
# - clipmenud
# - emacs

# manually starting stuff
# polybar nord &
sxhkd &
dunst &
udiskie --no-automount --notify --tray &

# enable compositor
picom -bGC

_JAVA_AWT_WM_NONREPARENTING=1
AWT_TOOLKIT=MToolkit

export _JAVA_AWT_WM_NONREPARENTING
export AWT_TOOLKIT

# the errors will be appended into ~/.xsesion-errors
while :; do
  openbox
  # ~/Sync/Projects/dwm/dwm
#  ~/Sync/Projects/cwm/cwm
done
