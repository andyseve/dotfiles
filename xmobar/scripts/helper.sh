#!/run/current-system/sw/bin/bash
# Author: Anish Sevekari
# Last Modified: Tue 31 Jan 2023 11:02:14 PM EST
# Helper functions for xmobar

xmobar_color(){
	# $1 color
	# $2 string
	color=$1
	text=$2
	return "<fc=$1>$2</fc>"
}


