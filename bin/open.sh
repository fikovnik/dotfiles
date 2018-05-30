#!/bin/sh

(setsid xdg-open "$@" >& /dev/null) & disown
