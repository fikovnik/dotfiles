#!/bin/bash

set -euo pipefail

if ! command chezmoi 2>&1; then
  sh -c "$(curl -fsLS get.chezmoi.io)" -- -b "$HOME/.local/bin"
  export PATH=$PATH:"$HOME/.local/bin"
fi

chezmoi init https://github.com/fikovnik/dotfiles
chezmoi apply
