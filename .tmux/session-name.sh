#!/bin/sh

if [ -z "$SSH_TTY" ]; then
  BG=default
else
  BG=red
fi

echo "#[fg=white, bg=$BG]#H:#S#[fg=default,bg=default]"
