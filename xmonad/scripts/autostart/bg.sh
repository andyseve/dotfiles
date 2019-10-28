#!/bin/zsh

wall_dir=$HOME/Pictures/wallpapers

[ ! -e $wall_dir ] && exit 1

mon=$(date +%m)
day=$(date +%d)

fallback_list=($(print -l $wall_dir/default*))
fallback=${fallback_list[1]}

daily_list=($(print -l $wall_dir/$mon/$day*))
daily=${daily_list[1]}

rand_list=($(print -l $wall_dir/other/*))
N=${#rand_list[@]}
((N=RANDOM%N))
rand=${rand_list[$N]}

if [[ -n "$daily" ]]; then
	feh --bg-scale $daily &
#elif [[ -n "$rand" ]]; then
	#feh --bg-scale $rand &
else
	feh --bg-scale $fallback &
fi
