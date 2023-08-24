#!/bin/bash

# set -euf -o pipefail

function hosts() {
	# Parse hostnames from /etc/hosts file
	hosts_file="/etc/hosts"
	grep -E -o '([[:alnum:]-]+\.)+[[:alnum:]-]+' "$hosts_file"

	# Parse hostnames from ~/.ssh/known_hosts file
	known_hosts_file="$HOME/.ssh/known_hosts"
	awk '!/^\|/ {print $1}' "$known_hosts_file" | cut -d',' -f1 | cut -d' ' -f1

	# Parse hostnames from ~/.ssh/config file
	ssh_config_file="$HOME/.ssh/config"
	awk '/^Host[[:space:]]/ && !/\*/ {print $2}' "$ssh_config_file"
}

function hosts_fzf() {
	hosts | uniq | grep -v 'localhost' | fzf -q "$1" --prompt="ssh> " --select-1
}

function ssh_menu() {
	res=$(hosts_fzf)
	if [ -n "$res" ]; then
		ssh -t "$res" zsh -ic '"tmux new-session -A -s main"'
	fi
}

if [ -t 1 ]; then
	ssh_menu
else
	~/bin/x-terminal.sh "$0"
fi
