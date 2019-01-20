#!/bin/sh
xrandr --output DP1-1 --off && \
  xrandr --output eDP1 --mode 2560x1440 --pos 0x0 --primary --output DP1-1 --mode 2560x1440 --pos 0x0
