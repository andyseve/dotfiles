#!/run/current-system/sw/bin/sh

pkill trayer;

# Code to use hight of xmobar window instead of a preset value
# xmobar0_pid=$(ps ax | grep xmobar.hs | grep -v grep | cut -d '?' -f 1 | tr -d ' ')
# while [ "$xmobar0_pid" = "" ]; do
# 	sleep 1;
# 	xmobar0_pid=$(ps ax | grep xmobar.hs | grep -v grep | cut -d '?' -f 1 | tr -d ' ')
# done
#
# xmobar0_window_id=$(xdotool search --pid $xmobar0_pid)
# while [ "$xmobar0_window_id" = "" ]; do
# 	sleep 1;
# 	xmobar0_window_id=$(xdotool search --pid $xmobar0_pid)
# done
#
# typeset -i xmobar0_height=$(xdotool getwindowgeometry $xmobar0_window_id | grep Geometry | cut -d ':' -f 2 | cut -d 'x' -f 2)
#
# trayer_height=$xmobar0_height-5

typeset -i trayer_height=30

trayer --edge top --align right --widthtype request --heighttype pixel --height $trayer_height --expand true --iconspacing 10 --padding 5 --distance 0 --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x00000000  --monitor 0
