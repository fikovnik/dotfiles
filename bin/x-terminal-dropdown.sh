#!/bin/bash -x

function position_window() {
    wid=$1

    eval $(xdotool getwindowgeometry --shell $wid)
    eval $(xwininfo -root | awk '/Width/{w=$2} /Height/{h=$2} END {print "screen_width="w"\nscreen_height="h}')
    x=$((screen_width/2-WIDTH/2))
    y=$((screen_height/2-HEIGHT/2))

    if [ ! $x -eq $X -o ! $y -eq $Y ]; then
         xdotool windowmove --sync $wid $x $y
         echo "Moving window $wid from [$X, $Y] to [$x, $y]"
    fi
}

function move_window_to_current_desktop() {
    wid=$1

    window_desktop=$(xdotool get_desktop_for_window $wid)

    if [ $window_desktop -ne $current_desktop ]; then
        echo "Moving window $wid from desktop $window_desktop to $current_desktop"
        xdotool set_desktop_for_window $wid $current_desktop
    fi
}

function find_window() {
    xdotool search "$@" --name dropdown 2>/dev/null | head -n 1
}

function start() {
    GEOMETRY=200x42
    (setsid /home/krikava/bin/x-terminal.sh -e tmux attach-session -t dropdown >& /dev/null) & disown
}

current_desktop=$(xdotool get_desktop)
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

    position_window $wid
else
    visible_wid=$(find_window --onlyvisible)
    if [ -z $visible_wid ]; then
        xdotool get_desktop_for_window $wid || xdotool windowmap --sync $wid
        window_desktop=$(xdotool get_desktop_for_window $wid 2>/dev/null)

        if [ $current_desktop -ne $window_desktop ]; then
            echo "Moving $wid"
            move_window_to_current_desktop $wid
            position_window $wid
            xdotool windowfocus --sync $wid
        else
            echo "Showing $wid"
            move_window_to_current_desktop $wid
            position_window $wid
            xdotool windowfocus --sync $wid
        fi
    else
        echo "Hiding $wid"
        xdotool windowunmap --sync $wid
    fi
fi
