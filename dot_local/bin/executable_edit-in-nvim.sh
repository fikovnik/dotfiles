#!/bin/bash

FILES=/tmp/edit-in-nvim-file-list

if [[ -f "$FILES" ]]; then
  files=$(cat "$FILES")
  rm "$FILES"
fi

/opt/homebrew/bin/nvim $files
