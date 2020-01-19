#!/usr/bin/env bash

echo $1
if [ $# -lt 1 ]; then
    echo "Usage: $0 name [ command ]"
    exit 1
fi

name=$1
shift

$HOME/bin/x-terminal.sh -d -n $name tmux new-session -A -s $name "$@"
