#!/bin/sh

rofi -show ssh -ssh-command '/home/krikava/bin/x-terminal.sh -e zsh -c "SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket) ssh {host} -t tmux new-session -A -s main"'
#rofi -show ssh -ssh-command '/home/krikava/bin/x-terminal.sh -e zsh -c "ssh {host} -t tmux new-session -A -s main"'
