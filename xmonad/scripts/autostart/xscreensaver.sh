#!/bin/sh

pidof gnome-screensaver > /dev/null && killall gnome-screensaver

xscreensaver -no-splash &
