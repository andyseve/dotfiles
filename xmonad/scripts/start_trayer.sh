#!/run/current-system/sw/bin/sh

pkill trayer;

xmobar0_pid=$(ps ax | grep xmobar.hs | grep -v grep | cut -d '?' -f 1 | tr -d ' ')
while [ "$xmobar0_pid" = "" ]; do
	sleep 1;
	xmobar0_pid=$(ps ax | grep xmobar.hs | grep -v grep | cut -d '?' -f 1 | tr -d ' ')
done

xmobar0_window_id=$(xdotool search --pid $xmobar0_pid)
while [ "$xmobar0_window_id" = "" ]; do
	sleep 1;
	xmobar0_window_id=$(xdotool search --pid $xmobar0_pid)
done

typeset -i xmobar0_height=$(xdotool getwindowgeometry $xmobar0_window_id | grep Geometry | cut -d ':' -f 2 | cut -d 'x' -f 2)

xmobar0_height=$xmobar0_height-2

trayer --edge top --align right --widthtype request --SetDockType true --transparent true --alpha 150 --heighttype pixel --height $xmobar0_height --tint 0x00000000 --iconspacing 5 --monitor 0
