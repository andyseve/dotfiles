#!/bin/sh

TOTAL="$(free -m | grep Mem | awk '{print $2}')"
USED="$(free -m | grep Mem | awk '{print $3}')"

PERC="$(( (100*"$USED") / "$TOTAL" ))"

mem_icon="8"
case "$PERC" in
	[0-9] )
		mem_icon="0" ;;
	1[0-9] )
		mem_icon="1" ;;
	2[0-9] )
		mem_icon="2" ;;
	3[0-9] )
		mem_icon="3" ;;
	4[0-9] )
		mem_icon="4" ;;
	5[0-9] )
		mem_icon="5" ;;
	6[0-9] )
		mem_icon="6" ;;
	7[0-9] )
		mem_icon="7" ;;
esac

printf '<action=`term -e htop` button=1>'
printf "<icon=ram/ram_%s.xpm/></action> %s/%sMB" "$mem_icon" "$USED" "$TOTAL"
