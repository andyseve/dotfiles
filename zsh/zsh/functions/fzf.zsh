# Author: Anish Sevekari
# Last Edited: Thu 01 Nov 2018 11:18:28 AM EDT
# Better fzf function
# Pipes through the command given as first arguement if it exists, and it not to be passed to fzf
# We prefer setting height to 40% for better readability

function fzf(){
	file=""
	if [[ -z "$1" ]]; then
		file="$(fzf-bin)"
	elif (( $+commands[$1] )); then
		if [[ -z "$2" ]]; then
			file="$(fzf-bin)"
		elif [[ $2 == -* ]]; then
			file="$(fzf-bin ${@:2})"
		else 
			file="$(fzf-bin -q ${@:2})"
		fi
		if [[ ! -z $file ]]; then
			$1 "$file"
			return 0
		fi
	elif [[ $1 == -* ]]; then
		file="$(fzf-bin ${@:1})"
	else
		file="$(fzf-bin -q ${@:1})"
	fi
	if [[ ! -z $file ]]; then
		echo "$file"
		return 0
	fi
	return 0
}
