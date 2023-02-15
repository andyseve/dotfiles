# Useful copy paste functions

# Copy path to clipboard
# dclip <dir> 
function dclip {
	if [ -e "$1" ]; then
		print -n $(realpath $1) | xclip -selection clipboard
	else
		print -n $PWD | xclip -selection clipboard
	fi

}

# Copy file to clipboard
# cclip <file>
function cclip {
	xclip -selection clipboard $1
}

# Paste contains of clipboard to a file
# cpaste <file>
function cpaste {
	if [ -f "$1" ]; then
		xclip -o > "$1"
	elif [ ! -e "$1" ]; then
		touch "$1"
		xclip -o > "$1"
	else
		xclip -o
	fi
}
