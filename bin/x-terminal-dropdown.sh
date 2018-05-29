#!/bin/bash

function move_window() {
    wid=$1

    eval $(xdotool getwindowgeometry --shell $wid)
    eval $(xwininfo -root | awk '/Width/{w=$2} /Height/{h=$2} END {print "D_WIDTH="w"\nD_HEIGHT="h}')
    x=$((D_WIDTH/2-WIDTH/2))
    y=$((D_HEIGHT/2-HEIGHT/2))

    echo "Moving window $wid to $x $y"

    xdotool windowmove $wid $x $y
}

function find_window() {
    xdotool search "$@" --name dropdown 2>/dev/null | head -n 1
}

function start() {
    GEOMETRY=200x42
    (setsid /home/krikava/bin/x-terminal.sh -e tmux attach-session -t dropdown >& /dev/null) & disown
}

wid=$(find_window)

if [ -z "$wid" ]; then
    echo "Window does not exists"
    start

    # wait for window
    for i in $(seq 3); do
        wid=$(find_window)
        [ -z "$wid" ] || break
        sleep .05
    done

    if [ -z "$wid" ]; then
        notify-send "Error" "Unable to find window for dropdown terminal"
        exit 1
    fi

    move_window $wid
else
    if [ -z "$(find_window --onlyvisible)" ]; then
        echo "Showing $wid"
        xdotool windowmap $wid
        xdotool windowfocus $wid
        move_window $wid
    else
        echo "Hiding $wid"
        xdotool windowunmap $wid
    fi
fi


