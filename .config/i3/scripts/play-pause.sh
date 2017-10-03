#!/bin/sh

case $(playerctl status) in

Playing)
  playerctl pause
  ;;

Paused)
  playerctl play
  ;;

*)
  exit;
esac  
