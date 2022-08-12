# Author: Anish Sevekari
# Last Modified: Fri 12 Aug 2022 08:56:59 AM EDT
# Function that syncs latex style file with the version in the git repository to update and keep changes
# Function calls diff with the main latex file
latexdiff() {
	STY="anishs.sty"
	BASE="$HOME/dotfiles/latex/anishs.sty"

	if [ ! -e "$BASE" ]; then
		echo "$BASE does not exist. Please pull from github."
	fi

	if [ -e "$1" ]; then
		STY="$1"
	elif [ ! -e "$STY" ]; then
		echo "$1 does not exists. Please check the style file."
	fi

	if check nvim; then
		nvim -d $STY $BASE
	elif check vim; then
		vimdiff $STY $BASE
	else
		echo "No suitable editor found to run diff."
	fi
}
