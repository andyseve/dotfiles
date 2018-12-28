# Author: Anish Sevekari
# Last Edited: Tue 13 Nov 2018 11:21:18 PM EST
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
		else
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
