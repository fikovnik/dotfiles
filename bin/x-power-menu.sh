#!/bin/bash

poweroff_command="systemctl poweroff"
reboot_command="systemctl reboot"
logout_command="i3-msg exit"
hibernate_command="systemctl hibernate"
suspend_command="systemctl suspend"

DMENU="rofi -dmenu -p system -i"
SESSION="cinnamon-session-quit --force --no-prompt"

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
        cinnamon-screensaver-command --lock
        ;;
    reboot)
        $SESSION --reboot
        ;;
    power-off)
        $SESSION --power-off
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
