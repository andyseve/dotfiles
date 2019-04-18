#!/bin/bash
## Last Modified: Thu 18 Apr 2019 14:39:53 UTC
## This script creates all the symlinks from correct folders
## Based on similar script by Chris Cox

################################################################################
# Global Variables #############################################################
################################################################################
CONFIG="$HOME/.config"
LOCAL="$HOME/.local/share"
DOTFILES="$HOME/dotfiles"
OVERWRITE="$HOME/.overwrites"
DID_OVERWRITE=false
EXISTS_OVERWRITE=false

################################################################################
# Helper Functinos #############################################################
################################################################################

# handling overwrites
if [ -e "$OVERWRITE" ]; then
	read -n 1 -p "Overwrite file already exists. Are you sure you want to proceed?[y|n]" cont
	if [ "$cont" = "n" ]; then
		exit 1
	else
		echo -e "\n$(date)" >> $OVERWRITE
		echo "--------------------------------------------------------------------------------" >> $OVERWRITE
		EXISTS_OVERWRITE=true
	fi
else
	touch $OVERWRITE
	echo $(date) >> $OVERWRITE
	echo "--------------------------------------------------------------------------------" >> $OVERWRITE
fi
		
# backs up if the directory exists
bup() {
	if [ -e "$1" ] || [ -L "$1" ]; then
		echo "creating backup of $1..."
		mv $1 $1.bak
		echo "$1" >> $OVERWRITE
		DID_OVERWRITE=true
	fi
}
# creates dir if it doesn't exist. 
cdir() {
	if [ ! -d "$1" ]; then
		echo "creating $1..."
		mkdir -p $1
	fi
}
# moves file/directory to new location (moves $1 to $2)
move() {
	if [ -e "$1" ]; then
		if [ -e "$2" ]; then
			echo "oops. $2 already exists, you should look into this"
			bup $2
			mv $1 $2
		else
			echo "moving $1 to $2..."
			mv $1 $2
		fi
	fi
}
# links $1 to $2 if it's not linked already
link() {
	if [ ! "$(readlink $2)" = "$1" ]; then
		bup $2
		echo "creating symlink $2 -> $1..."
		ln -s $1 $2
	fi
}
# checks if command exists and is executable
check() {
	if [ -x "$(command -v $1)" ]; then
		true
	else
		false
	fi
}
checkpkg() {
	if check apt; then
		if dpkg -l | grep -q $1; then
			true
		else
			false
		fi
	fi
}
# echos that command isn't installed
nope() {
	echo "$1 is not installed"
}
# cloning git repo $1 to folder $2
clone() {
	if [ -e "$2" ]; then
		echo "oops! directory $2 already exists. Can't clone in there."
	else
		echo "cloning $1 into $2"
		git clone $1 $2
	fi
}
# downloading file with url=$1 to $2
download() {
	if [ -e "$2" ]; then
		echo "file $2 already exists"
	else
		curl -fLo "$2" --create-dirs "$1"
	fi
}

################################################################################
# Configs ######################################################################
################################################################################

cdir $CONFIG/systemd/user
echo -e "\nlinking configs..."

#bash
if check bash; then
	link $DOTFILES/bash/bashrc $HOME/.bashrc
else
	nope bash
fi

#zsh
if check zsh; then
	IF=$DOTFILES/zsh
	OF=$HOME/.zsh
	FOLDERS=(aliases completions functions)

	cdir $OF

	link $IF/zshrc $OF/zshrc
	link $OF/zshrc $HOME/.zshrc
	for dir in ${FOLDERS[@]}; do
		link "$IF/$dir" "$OF/$dir"
	done

	clone "https://github.com/zplug/zplug.git" $OF/zplug
else
	nope zsh
fi

#vim
if check vim; then
	IF=$DOTFILES/vim
	OF=$HOME/.vim
	FOLDERS=(ftdetect ftplugin spell syntax UltiSnips)

	cdir $OF
	cdir $OF/.swp
	cdir $OF/.backup
	cdir $OF/.undo
	cdir $OF/view

	link $IF/vimrc $OF/vimrc
	link $OF/vimrc $HOME/.vimrc
	link $IF/ycm_extra_conf.py $HOME/.ycm_extra_conf.py
	link $IF/vimrc.noplugin $OF/vimrc.noplugin
	link $OF/vimrc.noplugin $HOME/.vimrc.noplugin
	for dir in ${FOLDERS[@]}; do
		link "$IF/$dir" "$OF/$dir"
	done

	cdir $OF/autoload
	download "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" "$OF/autoload/plug.vim"

else
	nope vim
fi

#neofetch
if check neofetch; then
	cdir $CONFIG/neofetch
	link $DOTFILES/neofetch/config.conf $CONFIG/neofetch/config.conf
else
	nope neofetch
fi

#latex

#cleaning up
if ! $DID_OVERWRITE; then
	echo "no overwrites! yaaay"
	if ! $EXISTS_OVERWRITE;then
		rm $OVERWRITE
	fi
else
	echo "check the list of overwrites in $OVERWRITE"
fi
