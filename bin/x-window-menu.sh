#!/bin/sh

rofi -show window \
     -show-icons \
     -p "> " \
     -kb-accept-entry '!Super-Tab,!Super-ISO_Left_Tab,!Super-Up,!Super-Down,Return' \
     -kb-row-down 'Super+Tab,Super+Down,Down' \
     -kb-row-up 'Super+ISO_Left_Tab,Super+Up,Up'
