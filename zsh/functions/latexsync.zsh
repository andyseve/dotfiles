# Author: Anish Sevekari
# Last Modified: Mon 07 Nov 2022 11:53:53 AM EST
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
