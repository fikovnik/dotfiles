#!/bin/bash

set -euo pipefail

if ! command -v chezmoi &>/dev/null; then
  sh -c "$(curl -fsLS get.chezmoi.io)" -- -b "$HOME/.local/bin"
  export PATH=$PATH:"$HOME/.local/bin"
fi

CHEZMOI=chezmoi
if [[ ! -t 0 ]]; then
  CHEZMOI="$CHEZMOI --force --no-tty"
fi

$CHEZMOI init https://github.com/fikovnik/dotfiles
$CHEZMOI apply
