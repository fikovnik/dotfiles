#!/bin/bash

poweroff_command="systemctl poweroff"
reboot_command="systemctl reboot"
logout_command="i3-msg exit"
hibernate_command="systemctl hibernate"
suspend_command="systemctl suspend"

DMENU="rofi -dmenu -p system -i"
SESSION="xfce4-session-logout"
LOCK="xflock4"

read -r -d '' OPTIONS << EOM
Lock
Logout
Suspend
Hibernate
Reboot
Power off
EOM

option=$(echo "$OPTIONS" | $DMENU | tr '[:upper:]' '[:lower:]' | tr ' ' '-')

case $option in
    lock)
        $LOCK
        ;;
    reboot)
        $SESSION --reboot
        ;;
    power-off)
        $SESSION --halt
        ;;
    logout)
        $SESSION --logout
        ;;
    suspend)
        systemctl suspend
        ;;
    hibernate)
        systemctl hibernate
        ;;
    *)
        ;;
esac
