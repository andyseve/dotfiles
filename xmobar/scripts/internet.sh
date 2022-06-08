#!/run/current-system/sw/bin/bash
# internet icon script for xmobar
# copied from https://github.com/Sloopy3333/dotfiles/blob/master/.config/xmonad/scripts/internet.sh

wifistatus="$(cat /sys/class/net/w*/operstate)"
ethstatus="$(cat /sys/class/net/e*/operstate)"
color_normal="#93a1a1"
color_red="#dc322f"
color_green="#859900"

if [ $wifistatus = "up" ]; then 
    essid="$(nmcli c | sed -n '2{p;q}' | awk '{print $1}')"
    typeset -i quality="$(cat /proc/net/wireless |  sed -n '3{p;q}' | awk '{printf "%.0f\n",$3}')"
    icon=""
		if [ $quality -le 20 ]; then
			color=$color_red
		elif [ $quality -ge 50 ]; then
			color=$color_green
		else
			color=$color_normal
		fi
elif [ $ethstatus = "up" ]; then
    essid="$(nmcli c | sed -n '2{p;q}' | awk '{print $5}')"
    quality=""
    icon=""
		color=$color_green
else
    essid="Disconnected"
    quality=""
    icon=""
		color=$color_red
fi

echo "<fc=$color><fn=1>$quality</fn>$icon</fc>"
