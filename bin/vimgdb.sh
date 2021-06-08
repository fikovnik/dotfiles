#!/bin/bash

set -e

function error {
  1>&2 echo "$@"
  exit 1
}

usage="
Usage: vimgdb [options] [program [arguments]]

Start vim with :Termdebug

Options:
  -h, --help            Print short help message and exit
  --version             Print version info and exit
  -p PID                Attach to given PID
"

program=""
args=""

while [[ $# -gt 0 ]]; do
  case $1 in
    -h|--help)
      echo "$usage"
      exit 0
      ;;
    --version)
      echo "valgrind"
      exit 0
      ;;
    *)
      if [[ -z $program ]]; then
          program="$1"
          [[ -x $program ]] || error "$1: is not executable"
      else
          args="$args $1"
      fi
      shift
      ;;
  esac
done

if [[ -n $args ]]; then
  cmd="+'TermdebugCommand $program $args'"
else
  cmd="+'Termdebug $program'"
fi

vim --cmd "let g:rooter_manual_only=1" \
  +'packadd termdebug' \
  +"cd $(pwd)" +"TermdebugCommand $program $args"
