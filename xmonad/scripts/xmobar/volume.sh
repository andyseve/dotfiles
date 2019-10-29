#!/bin/sh

vol=$(amixer get Master | grep Playback | awk -F'[]%[]' '{print $2}' | tail -n 1)

#printbar() {
	#printf '%*s' "$1" | tr ' ' "$2"
#}

vol_icon="3"
case "$vol" in
	[0-9]|1[0-9]|2[0-4] )
		vol_icon="0" ;;
	2[5-9]|[3-4][0-9] )
		vol_icon="1" ;;
	[5-6][0-9]|7[0-4] )
		vol_icon="2" ;;
esac

amixer get Master | grep -q '\[off\]' && vol_icon="mute" 

printf '<action=`pavucontrol` button=1>'
printf "<icon=volume/volume_%s.xpm/></action>%s%%" "$vol_icon" "$vol"
