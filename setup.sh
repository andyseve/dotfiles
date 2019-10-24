## Last Modified: Thu 24 Oct 2019 11:09:59 AM EDT
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
LITE=false
NOPLUGIN=false

while test $# != 0
do
	case "$1" in
		-l | --lite)
			LITE=true
			;;
		-n | --noplugin)
			NOPLUGIN=true
			;;
		*)
			echo "flags are --lite,--noplugin"
			exit 0
			;;
	esac
	shift
done

# handling overwrites
if [ -e "$OVERWRITE" ]; then
	while true; do
		read -p "Overwrite file already exists. Are you sure you want to run setup again?[y|n]" cont
		printf "\n"
		case $cont in
			[Yy]*)
				echo "$(date)" >> $OVERWRITE
				echo "--------------------------------------------------------------------------------" >> $OVERWRITE
				EXISTS_OVERWRITE=true
				break;;
			[Nn]*)
				echo "Exiting."
				exit 0
				;;
			*)
				echo "Please enter a valid selection"
				;;
		esac
	done
else
	touch $OVERWRITE
fi


# import helper functions
. $DOTFILES/zsh/functions/helper.zsh



################################################################################
# Configs ######################################################################
################################################################################

cdir $CONFIG/systemd/user
echo -e "\nlinking configs..."

# bash
if check bash; then
	link $DOTFILES/bash/bashrc $HOME/.bashrc
else
	nope bash
fi

# zsh
if check zsh; then
	IF=$DOTFILES/zsh
	OF=$HOME/.zsh
	FOLDERS=(aliases functions)

	cdir $OF

	link $IF/zshrc $OF/zshrc
	link $IF/zshrc.lite $OF/zshrc.lite
	link $IF/zshrc.noplugin $OF/zshrc.noplugin
	link $IF/zshrc.testing $OF/zshrc.testing

	for dir in ${FOLDERS[@]}; do
		link "$IF/$dir" "$OF/$dir"
	done

	if $LITE; then
		link $OF/zshrc.lite $HOME/.zshrc
	elif $NOPLUGIN; then
		link $OF/zshrc.noplugin $HOME/.zshrc
	else
		link $OF/zshrc $HOME/.zshrc
	fi

	# zplugin setup
	if [ -z "$ZPLG_HOME" ]; then
		ZPLG_HOME="${ZDOTDIR:-$HOME}/.zplugin"
	fi
	cdir $ZPLG_HOME
	chmod g-rwX $ZPLG_HOME
	if test -d "$ZPLG_HOME/bin/.git"; then
		cd "$ZPLG_HOME/bin"
		git pull origin master
	else
		cd "$ZPLG_HOME"
		git clone --depth 10 https://github.com/zdharma/zplugin.git bin
	fi
else
	nope zsh
fi

# vim
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
	link $IF/vimrc.noplugin $OF/vimrc.noplugin
	link $IF/vimrc.lite $OF/vimrc.lite
	link $IF/vimrc.testing $OF/vimrc.testing
	link $IF/ycm_extra_conf.py $HOME/.ycm_extra_conf.py
	for dir in ${FOLDERS[@]}; do
		link "$IF/$dir" "$OF/$dir"
	done

	if $LITE; then
		link $OF/vimrc.lite $HOME/.vimrc
	elif $NOPLUGIN; then
		link $OF/vimrc.noplugin $HOME/.vimrc
	else
		link $OF/vimrc $HOME/.vimrc
	fi

	cdir $OF/autoload
	download "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" "$OF/autoload/plug.vim"
else
	nope vim
fi

# neofetch
if check neofetch; then
	cdir $CONFIG/neofetch
	link $DOTFILES/neofetch/config.conf $CONFIG/neofetch/config.conf
else
	nope neofetch
fi

#latex
if check pdflatex; then
	cdir $HOME/texmf/tex/latex/local
	link $DOTFILES/latex/anishs.sty $HOME/texmf/tex/latex/local/anishs.sty
fi

# rtorrent
if check rtorrent; then
	IF="$DOTFILES/rtorrent"
	OF="$HOME/torrents"

	link $IF/rtorrent.rc $HOME/.rtorrent.rc

	FOLDERS=(.session log incomplete)
	STORAGE=(downloads watch)
	SUBDIRS=(games iso movies music series)

	for fol in ${FOLDERS[@]}; do
		cdir "$OF/$fol"
	done
	for fol in ${STORAGE[@]}; do
		for subdir in ${SUBDIRS[@]}; do
			cdir "$OF/$fol/$subdir"
		done
	done

	link $IF/rtorrent.service $CONFIG/systemd/user/rtorrent.service
else
	nope rtorrent
fi

# xmonad
if check xmonad; then
	IF="$DOTFILES/xmonad"
	OF="$HOME/.xmonad"

	link $IF/xmonad.hs $OF/xmonad.hs
else
	nope xmonad
fi


################################################################################
# Themes #######################################################################
################################################################################
while true; do
	read -p "Do you want to install themes[y|n]" install_themes
	printf "\n"
	case $install_themes in
		[Yy]*)
			echo "Installing themes..."
			$HOME/dotfiles/.setup_themes.sh
			break;;
		[Nn]*)
			echo "You can install themes later from $HOME/dotfiles/.setup_themes.sh"
			echo "Or install themes separately in $HOMe/dotfiles/themes"
			break;;
		*)
			echo "Please enter a valid selection"
			;;
	esac
done


# cleaning up
if ! $DID_OVERWRITE; then
	echo "no overwrites! yaaay"
	if ! $EXISTS_OVERWRITE;then
		rm $OVERWRITE
	fi
else
	echo "Overwritten files:"
	cat $OVERWRITE
fi
