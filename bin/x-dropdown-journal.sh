#!/usr/bin/env bash

$HOME/bin/x-dropdown.sh -t journal -g 142x42 tmux new-session -A -s journal nvim +'cd ~/Notes/Journal' +"e $(date +'%Y-W%V.md')"
