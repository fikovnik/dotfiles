#!/bin/bash

DIR=~/.screenlayout

LAYOUT=$(find $DIR -type f | sed 's=.*/==;s=\.[^.]*$==' | rofi -dmenu -p "Screen Layout: ")

[ -z "$LAYOUT" ] && exit

$DIR/$LAYOUT.sh

exit
