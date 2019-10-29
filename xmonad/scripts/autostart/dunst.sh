#!/bin/sh

if systemctl --user status xfce4-notifyd | grep -q running; then
	systemctl --user stop xfce4-notifyd
fi

dunst &
