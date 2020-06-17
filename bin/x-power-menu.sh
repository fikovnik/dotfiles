#!/bin/bash

cmd_lock="xscreensaver --lock"
cmd_logout="pkill xinit"
cmd_suspend="systemctl suspend"
cmd_hibernate="systemctl hibernate"
cmd_reboot="systemctl reboot"
cmd_poweroff="systemctl poweroff"
cmd_restartwm="pkill dwm"

DMENU="rofi -dmenu -p system -i"

read -r -d '' OPTIONS << EOM
Lock
Logout
Suspend
Hibernate
Reboot
Power off
Restart WM
EOM

option=$(echo "$OPTIONS" | $DMENU | tr '[:upper:]' '[:lower:]' | tr ' ' '-')

case $option in
    lock)
        $cmd_lock
        ;;
    logout)
        $cmd_logout
        ;;
    suspend)
        $cmd_suspend
        ;;
    hibernate)
        $cmd_hibernate
        ;;
    reboot)
        $cmd_reboot
        ;;
    power-off)
        $cmd_poweroff
        ;;
    restart-wm)
        $cmd_restartwm
        notify-send "WM restarted"
        ;;
    *)
        ;;
esac
