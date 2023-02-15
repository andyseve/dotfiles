#!/run/current-system/sw/bin/bash
# Author: Anish Sevekari
# Last Modified: Wed 15 Feb 2023 01:48:57 PM EST
# Helper functions for xmobar

# xmobar <color> <string>
xmobar_color(){
	color=$1
	text=$2
	return "<fc=$1>$2</fc>"
}

# xmobar <string> <actions> <buttons>


