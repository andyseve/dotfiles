# Author: Anish Sevekari
# Last Modified: Fri 13 Jan 2023 10:21:22 AM EST

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

# Function that creates a diff using git-latexdiff
pdfdiff() {
	MAIN="main.tex"
	DIFF="git-latexdiff"

	if [ ! -e "$MAIN" ]; then 
		echo "$MAIN does not exist. Please provide the correct filename."
	fi

	if check $DIFF; then
		$DIFF --main $MAIN -o diff.pdf --latexmk --cleanup keeppdf $1
	else
		echo "Latexdiff is not installed. Please install git-latexdiff and try again."
	fi
}
