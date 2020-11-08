#!/bin/sh

#!/bin/sh
xrandr --output DP-1 --off
xrandr --output DP-2 --off
xrandr --output DP-1-1 --off
xrandr --output DP-1-2 --off
xrandr --output DP-1-3 --off
xrandr --output eDP-1 --primary --mode 2560x1440 --pos 0x0 --rotate normal
xrandr --output DP-1-1 --mode 2560x1440 --pos 0x0 --rotate normal
