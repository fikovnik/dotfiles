#!/bin/sh -x

if tmux list-sessions | grep dropdown | grep attached; then
    i3-msg "scratchpad show"
else
    $HOME/bin/x-terminal.sh tmux new-session -A -s dropdown
fi
