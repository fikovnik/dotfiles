#!/bin/sh

DIR=~/.openvpn

if [ $# -eq 1 ]; then
    VPN_NAME="$1"
    VPN="$DIR/$VPN_NAME.ovpn"
    if [ ! -f $VPN ]; then
        notify-send "Unknown file: $VPN"
        exit;
    fi

    if ps a | grep "openvpn $VPN"; then
        notify-send "openvpn $VPN is already running"
        coproc (i3-msg workspace "9: " & > /dev/null 2>&1)
        exit;
    fi

    coproc (i3-msg workspace "9: " & urxvt -title "OpenVPN: $VPN" -e /bin/bash -c "sudo openvpn $VPN" & > /dev/null 2>&1)
    exit;
else
    find $DIR -type f -name "*.ovpn" | sed 's=.*/==;s=\.[^.]*$=='
fi

exit;
