#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title New tmux session
# @raycast.mode silent

open -a Ghostty "$(which tmux)" "$@"
