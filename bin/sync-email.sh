#!/bin/bash

echo "Checking internet connection..."
if ! ping -qnc1 -w4 8.8.8.8 >/dev/null 2>&1; then
  notify-send "No internet connection"
  exit 1
fi

echo "Sending mail in outbox..."
PATH=~/.local/bin/:$PATH msmtp-queue -r
if [ $? -ne 0 ]; then
  notify-send "There was a problem processing outbox"
fi

echo "Syncing gmail account..."
pushd . > /dev/null
cd ~/Mail/krikava@gmail.com
gmi sync
if [ $? -ne 0 ]; then
  notify-send "There was a problem synchronizing gmail"
fi
popd > /dev/null

# sync drafts
#mbsync gmail-drafts
#if [ $? -ne 0 ]; then
#  notify-send "There was a problem synchronizing gmail drafts"
#fi

echo "Syncing notmuch..."
notmuch new
if [ $? -ne 0 ]; then
  notify-send "There was a problem updating notmuch"
fi
