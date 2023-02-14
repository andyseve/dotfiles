# Author: Anish Sevekari
# Last Modified: Mon 07 Nov 2022 05:12:12 PM EST
# Function that adds arxiv bib info from pdfs
# Opens all the files added in edit mode to change the name

pubsarxiv(){
	dir=$(pwd)
	wdir=""

	if [[ ! -e "$1" || -d "$1" ]] then
		if [[ -d "$1" ]] then
			cd $1
		fi
		for file in *.pdf
		do
			arxiv=${file%.pdf}
			output=$(pubs add --arxiv $arxiv --docfile $file -M)
			pubskey=${output#*\[}
			pubskey=${pubskey%\]*}
		done

		# TODO: Start edits, but finish rest of the process in background
		pubs edit $pubskey 

		# Reset dir
		cd $dir
	else
		file=$1
		arxiv=${file%.pdf}
		output=$(pubs add --arxiv $arxiv --docfile $file -M)
		pubskey=${output#*\[}
		pubskey=${pubskey%\]*}
		pubs edit $pubskey
	fi
}
