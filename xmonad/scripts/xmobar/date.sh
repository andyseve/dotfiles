#!/bin/sh

cur_day=$(date +%d)
cur_time=$(date +"%I:%M %p")




printf '<action=`lxterminal -e ikhal` button=1>'
printf "<icon=calendar/%s.xpm/></action> %s" "$cur_day" "$cur_time"
