#!/bin/sh

if [ $# -ne 1 ]; then
  echo "Usage: $0 <node>"
  exit 1
fi

node="$1"

case "$node" in
  node-200)
    port="8788"
    ;;
  node-202)
    port="8789"
    ;;
  *)
    echo "Unknown node $node"
    exit 1
    ;;
esac

tmux list-session | grep "rstudio-$node" || \
  tmux new-session -d -s "rstudio-$node" ssh -nNT -L $port:localhost:$port $node 
