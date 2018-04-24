#!/bin/sh

notmuch new
if [ $? -ne 0 ]; then
  notify-send "There was a problem updating notmuch"
  exit 1
fi
