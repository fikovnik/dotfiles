#!/bin/sh

curr=$(xbacklight -get)

function restore {
    xbacklight -set $curr
}

trap restore SIGINT SIGTERM SIGKILL

xbacklight -time 5000 -set 0

restore
