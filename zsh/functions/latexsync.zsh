# Author: Anish Sevekari
# Last Modified: Wed 17 Aug 2022 12:50:58 PM EDT
# Function that syncs latex style file with the version in the git repository to update and keep changes
# Function calls diff with the main latex file
latexsync() {
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
