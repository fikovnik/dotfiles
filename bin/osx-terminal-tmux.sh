#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title New tmux session
# @raycast.mode silent

alacritty msg create-window -e $(which tmux) new-session "$@" || alacritty -e $(which tmux) new-session "$@"
