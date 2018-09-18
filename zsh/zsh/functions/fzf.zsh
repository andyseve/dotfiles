# Author: Anish Sevekari
# Last Edited: Mon 17 Sep 2018 10:30:22 PM EDT
# Better fzf function
# Pipes through the command given as first arguement if it exists, and it not to be passed to fzf
# We prefer setting height to 40% for better readability

function fzf(){
	file=""
	if [[ -z "$1" ]]; then
		file="$(fzf-bin --height 40% --reverse)"
	elif (( $+commands[$1] )); then
		if [[ -z "$2" ]]; then
			file="$(fzf-bin --height 40% --reverse)"
		elif [[ $2 == -* ]]; then
			file="$(fzf-bin ${@:2})"
		else 
			file="$(fzf-bin --height 40% --reverse -q ${@:2})"
		fi
		if [[ ! -z $file ]]; then
			$1 "$file"
			return 0
		fi
	elif [[ $1 == -* ]]; then
		file="$(fzf-bin ${@:1})"
	else
		file="$(fzf-bin --height 40% --reverse -q ${@:1})"
	fi
	if [[ ! -z $file ]]; then
		echo "$file"
		return 0
	fi
	return 0
}
