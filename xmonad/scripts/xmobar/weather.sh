#!/bin/sh

weather_tmp=$(sed -n '1p' "$HOME/.local/share/weather_report_trunc")
weather_cond=$(sed -n '$p' "$HOME/.local/share/weather_report_trunc") 

case "$weather_cond" in
	"overcast" )
		cond_icon="cloudy" ;;
	"partly cloudy" )
		cond_icon="partly_cloudy" ;;
	"sunny" )
		cond_icon="sunny" ;;
	"clear" )
		cond_icon="clear" ;;
	"rain"*|"heavy rain"* )
		cond_icon="rain" ;;
	"thunder"* )
		cond_icon="thunder" ;;
	"heavy snow" )
		cond_icon="heavy_snow" ;;
	"light snow" )
		cond_icon="light_snow" ;;
	"light rain"* )
		cond_icon="light_rain" ;;
	"patchy rain"* )
		cond_icon="light_drizzle" ;;
	"mist"|"fog" )
		cond_icon="cloudy" ;;
	* )
		cond_icon="sunny" ;;
esac

#case "$weather" in
	#*[Uu]nknown*|*[Ee]rror* )
		#weather="ERROR" ;;
#esac


printf '<action=`alacritty -e weather` button=1>'
printf "<icon=weather/%s.xbm/> " "$cond_icon"
case "$(cat $HOME/.local/share/weather_report_trunc)" in
	*[Uu]nknown*|*[Ee]rror* )
		printf 'ERROR' ;;
	* )
		printf "%s %s" "$weather_tmp" "$weather_cond" ;;
esac
printf "</action>"

#printf "<icon=weather/%s.xbm/></action> %s %s" "$cond_icon" "$weather_tmp" "$weather_cond"
