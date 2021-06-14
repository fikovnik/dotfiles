#!/bin/sh

DIR=~/.screenlayout

LAYOUT=$(find $DIR -type f | sed 's=.*/==;s=\.[^.]*$==' | rofi -dmenu -p "Screen Layout")

[ -z "$LAYOUT" ] && exit

$DIR/$LAYOUT.sh

if [ -z "$XDG_CURRENT_DESKTOP" ]; then
  feh --bg-scale ~/Documents/Pictures/Wallpapers/dolomites.jpg
  ~/.config/polybar/launch.sh
fi

exit
