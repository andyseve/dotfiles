# Author: Anish Sevekari
# Last Edited: Sat 09 Jun 2018 01:57:48 AM DST
# Better fzf function
# Pipes through the command given as first arguement if it exists, and it not to be passed to fzf
function fzf(){
	if [ -n $1 ]; then
		# If the first arguement does not start with - then pipe through
		if [[ $1 == -* ]]; then
			fzf-bin ${@:1}
		else
			if [ -n $2 ]; then
				if [[ $2 == -* ]]; then
					$1 "$(fzf-bin --height 40% --reverse ${@:2})"	# setting height to 40% makes it less distracting

				else
					$1 "$(fzf-bin -q $2 --height 40% --reverse ${@:3})"
				fi
			else
				$1 "$(fzf-bin --height 40% --reverse)"
			fi
		fi
	else
		fzf-bin --height 40% --reverse
	fi
}

