#!/bin/sh

OPTIONS="Lock screen\nExit WM\nReboot system\nPower-off system\nSuspend system\nHibernate system"
LOCK="$HOME/.config/i3/scripts/lock.sh"

option=$(echo -e $OPTIONS | rofi -dmenu -p "exit>"  | cut -f 1 -d' ' | tr -d '\r\n')
if [ ${#option} -gt 0 ]; then
    case $option in
        Lock)
            $LOCK
            ;;
        Exit)
            i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'
            ;;
        Reboot)
            systemctl reboot
            ;;
        Power-off)
            systemctl poweroff
            ;;
        Suspend)
            $LOCK; systemctl suspend
            ;;
        Hibernate)
            $LOCK; systemctl hibernate
            ;;
        *)
            ;;
    esac
fi
