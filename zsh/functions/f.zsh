# Author: Anish Sevekari
# Last Edited: Fri 21 Jun 2019 02:32:35 PM EDT
# Better fzf function
# Pipes through the command given as first arguement if it exists, and it not to be passed to fzf
# We prefer setting height to 40% for better readability

function f(){
	file=""
	if [[ -z "$1" ]]; then
		file="$(fzf)"
	elif (( $+commands[$1] )); then
		if [[ -z "$2" ]]; then
			file="$(fzf)"
		elif [[ $2 == -* ]]; then
			file="$(fzf ${@:2})"
		else 
			file="$(fzf -q ${@:2})"
		fi
		if [[ ! -z $file ]]; then
			$1 "$file"
			return 0
		else
			return 0
		fi
	elif [[ $1 == -* ]]; then
		file="$(fzf ${@:1})"
	else
		file="$(fzf -q ${@:1})"
	fi
	if [[ ! -z $file ]]; then
		echo "$file"
		return 0
	fi
	return 0
}
