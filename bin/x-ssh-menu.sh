#!/bin/sh

rofi -show ssh -ssh-command '/home/krikava/bin/x-terminal.sh -e sh -c "export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket); ssh {host} -t tmux new-session -A -s main"'
