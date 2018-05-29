#!/bin/sh

GEOMETRY=${GEOMETRY:-142x42}

/usr/local/bin/st -g $GEOMETRY "$@"
