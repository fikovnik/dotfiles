#!/bin/sh

DIR=~/.screenlayout

LAYOUT=$(find $DIR -type f | sed 's=.*/==;s=\.[^.]*$==' | rofi -dmenu -p "Screen Layout")

[ -z "$LAYOUT" ] && exit

$DIR/$LAYOUT.sh

if [ -z "$XDG_CURRENT_DESKTOP" ]; then
  res=$(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/')
  background="~/.local/share/backgrounds/dolomites-$(res).jpg"
  [ -f "$background" ] && feh --bg-scale "$background"
  ~/.config/polybar/launch.sh
fi

exit
