#!/bin/bash

N=${1:-5}
CURR=$(brightnessctl -m g | cut -d',' -f 3)
STEP=$(( $CURR / $N ))

function restore {
    echo "Restoring to $CURR"
    brightnessctl s $CURR
}

trap restore SIGINT SIGTERM SIGKILL

for i in $(seq 1 $N); do
    brightnessctl s $STEP-
    sleep 1
done

restore
