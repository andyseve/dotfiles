#!/bin/bash
## Last edited on:
## This script creates all the symlinks from correct folders
## Based on similar script by Chris Cox

################################################################################
# Global Variables #############################################################
################################################################################
CONFIG="$HOME/.config"
LOCAL="$HOME/.local/share"
DOTFILES="$HOME/dotfiles"
OVERWRITE="$HOME/.overwrites"
DID_OVERWRITE="false"

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
	fi
	DID_OVERWRITE="true"
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
		DID_OVERWRITE="true"
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
	link $DOTFILES/zsh/zshrc $HOME/.zshrc
	link $DOTFILES/zsh/zsh $HOME/.zsh
else
	nope zsh
fi

#vim
if check vim; then
	link $DOTFILES/vim/vim $HOME/.vim
	link $DOTFILES/vim/vimrc $HOME/.vimrc
	
	cdir $HOME/.vim/.backup
	cdir $HOME/.vim/.swp
	cdir $HOME/.vim/.undo
	cdir $HOME/.vim/view
else
	nope vim
fi

#latex

#cleaning up
if [ !$DID_OVERWRITE ]; then
	rm $OVERWRITE
else
	echo "check the list of overwrites in $OVERWRITE"
fi
