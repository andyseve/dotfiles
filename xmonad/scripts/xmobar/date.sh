#!/bin/sh

cur_day=$(date +%d)
cur_time=$(date +"%a %b %_d %H:%M")




printf '<action=`alacritty -e task` button=1>'
printf "%s" "$cur_time </action>"
