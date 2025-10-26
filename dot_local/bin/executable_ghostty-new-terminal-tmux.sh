#!/bin/bash

cd ~ || exit
exec /opt/homebrew/bin/tmux new-session -A -c ~ -s main
