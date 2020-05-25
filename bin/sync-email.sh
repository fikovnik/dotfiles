#!/bin/sh

function error_tty {
  >&2 echo "$@"
}

function error_notify {
  notify-send "$@"
}

if [ -n "$TERM" ]; then
  offlineimap_ui="ttyui"
  error=error_tty
else
  offlineimap_ui="ttyui"
  error=error_notify
fi

/usr/bin/offlineimap -o -u $offlineimap_ui
exitval=$?
if [ $exitval -ne 0 ]; then
   "$error" "There was a problem syncing email"
   exit $exitval
fi

#notmuch new
#if [ $? -ne 0 ]; then
#  notify-send "There was a problem updating notmuch"
#  exit 1
#fi
