#!/bin/bash

AUTOSTART="$HOME/.xmonad/scripts/autostart"

for PROG in $AUTOSTART/*.sh; do
	$PROG
done
