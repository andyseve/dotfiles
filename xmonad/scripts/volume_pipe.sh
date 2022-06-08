#!/run/current-system/sw/bin/sh


status="$(amixer sget Master | grep -o -m 1 "\[[a-z]*\]" | tr -d '%[]')"
vol="$(amixer sget Master | grep -o -m 1 "[[:digit:]]*%" | tr -d '%')"

pipe_volume() {
	color_normal="#93a1a1"
	color_red="#dc322f"
	color_orange="#cb4b16"
	color_green="#859900"
	color=$color_normal

	if [ $status = off ]; then
		vol="0"
		color=$color_red
	fi

	if [ "$vol" = "0" ]; then
		icon="ﱝ "
		color=$color_red
	elif [ "$vol" -lt "20" ]; then
		icon="奄"
		color=$color_green
	elif [ "$vol" -lt "50" ]; then
		icon="奔"
		color=$color_normal
	else
		icon="墳"
		color=$color_orange
	fi
	echo "<fc=$color><fn=1>$vol</fn>$icon</fc>" | tee /tmp/volume_pipe
	echo "<fc=$color><fn=1>$vol</fn>$icon</fc>" | tee /tmp/volume_pipe
	echo "<fc=$color><fn=1>$vol</fn>$icon</fc>" | tee /tmp/volume_pipe
}

send_notification() {
	bar=$(seq -s "─" $(($vol / 5)) | sed 's/[0-9]//g')
	icon="notification-audio-volume-medium"
	urgency="normal"
	if [ "$vol" = "0" ]; then
		icon="notification-audio-volume-muted"
	elif [ "$vol" -lt "20" ]; then
		icon="notification-audio-volume-low"
	elif [ "$vol" -lt "50" ]; then
		icon="notification-audio-volume-medium"
	else
		icon="notification-audio-volume-high"
		urgency="critical"
	fi
	notify-send -a "volume" -u "$urgency" -i "$icon" -t 2000 -r 31415 -h int:value:"$vol" -h string:synchronous:"$bar" "volume"
}

case $1 in
	"up")
		amixer set Master 5%+ unmute > /dev/null
		;;
	"down")
		amixer set Master 5%- unmute > /dev/null
		;;
	"toggle")
		amixer set Master "toggle" > /dev/null
		;;
	"")
		pipe_volume
		exit 0
		;;
esac

pipe_volume
send_notification
