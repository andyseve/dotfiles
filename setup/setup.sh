## Last Modified: Sat Dec 11 12:00:18 2021
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

	link $IF/dircolors $OF/dircolors

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

	# zinit setup
	if [ -z "$ZINIT_HOME" ]; then
		ZINIT_HOME="${ZDOTDIR:-$HOME}/.zinit"
	fi
	cdir $ZINIT_HOME
	chmod g-rwX $ZINIT_HOME
	if test -d "$ZINIT_HOME/bin/.git"; then
		cd "$ZINIT_HOME/bin"
		git pull origin master
	else
		cd "$ZINIT_HOME"
		git clone https://github.com/zdharma-continuum/zinit.git bin
	fi
else
	nope zsh
fi

# bin

while true; do
	read -p "Do you want to install scripts in bin[y|n]" install_bin
	printf "\n"
	case $install_bin in
		[Yy]*)
			IF=$DOTFILES/bin
			OF=$HOME/bin

			cdir $OF

			for file in "$IF"/*; do
				link $file $OF/${file##*/}
			done
			break;;
		[Nn]*)
			break;;
		*)
			echo "Please enter a valid selection"
			;;
	esac
done

# git
if check git; then
	link $DOTFILES/git/gitconfig $HOME/.gitconfig
fi

# alacritty
if check alacritty; then
	IF=$DOTFILES/alacritty
	OF=$HOME/.config/alacritty

	link $IF/alacritty.yml $OF/alacritty.yml
else
	nope alacritty
fi

# vim
if check vim; then
	IF=$DOTFILES/vim
	OF=$HOME/.vim
	FOLDERS=(ftdetect ftplugin spell syntax config core ultisnips)

	cdir $OF
	cdir $OF/.swp
	cdir $OF/.backup
	cdir $OF/.undo
	cdir $OF/view
	cdir $CONFIG/coc/ultisnips

	link $IF/vimrc $OF/vimrc
	link $IF/vimrc.noplugin $OF/vimrc.noplugin
	link $IF/vimrc.lite $OF/vimrc.lite
	link $IF/vimrc.testing $OF/vimrc.testing
	link $IF/ycm_extra_conf.py $HOME/.ycm_extra_conf.py
	link $IF/coc-settings.json $OF/coc-settings.json
	link $IF/ultisnips $CONFIG/coc/ultisnips
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

# nvim
if check nvim; then
	IF=$DOTFILES/nvim
	OF=$HOME/.config/nvim
	FOLDERS=(ftdetect ftplugin spell syntax config core ultisnips lua)
	
	cdir $OF
	link $IF/init.vim $OF/init.vim
	link $DOTFILES/vim/coc-settings.json $OF/coc-settings.json
	for dir in ${FOLDERS[@]}; do
		link "$IF/$dir" "$OF/$dir"
	done
else
	nope neovim
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
	OF="$CONFIG/xmonad"
	cdir $OF

	link $IF/xmonad.hs $OF/xmonad.hs
else
	nope xmonad
fi

# xmobar
if check xmobar; then
	IF="$DOTFILES/xmobar"
	OF="$CONFIG/xmobar"
	cdir $OF
	
	if [ ! -L $OF/xmobar.hs ]; then
		echo "Choose the xmobar file to link"
		select opt in $IF/*.hs
		do
			link $opt $OF/xmobar.hs
			break
		done
	fi
	link $IF/my-xmobar.cabal $OF/my-xmobar.cabal
	link $IF/shell.nix $OF/shell.nix
	link $IF/hie.yaml $OF/hie.yaml
else
	nope xmobar
fi

# xfce
if check xfce4-terminal; then
	cdir $HOME/.config/xfce4/terminal
	link $DOTFILES/xfce4/terminal/terminalrc $HOME/.config/xfce4/terminal/terminalrc
else
	nope xfce4-terminal
fi

# ranger
if check ranger; then
	IF="$DOTFILES/ranger"
	OF="$HOME/.config/ranger"
	cdir $OF
	
	link $IF/rc.conf $OF/rc.conf
	link $IF/commands.py $OF/commands.py
else
	nope ranger
fi

# rofi
if check rofi; then
	IF="$DOTFILES/rofi"
	OF="$HOME/.config/rofi"
	cdir $OF

	link $IF/config.rasi $OF/config.rasi
else
	nope rofi
fi

# zathura
if check zathura; then
	IF="$DOTFILES/zathura"
	OF="$HOME/.config/zathura"
	cdir $OF

	link $IF/zathurarc $OF/zathurarc
else
	nope zathura
fi

# dunst
if check dunst; then
	IF="$DOTFILES/dunst"
	OF="$HOME/.config/dunst"
	cdir $OF

	link $IF/dunstrc $OF/dunstrc
else
	nope dunst
fi

# picom
if check picom; then
	IF="$DOTFILES/picom"
	OF="$HOME/.config/picom"
	cdir $OF

	link $IF/picom.conf $OF/picom.conf
else
	nope picom
fi

# vdirsyncer
if check vdirsyncer; then
	IF="$DOTFILES/vdirsyncer"
	OF="$HOME/.config/vdirsyncer"
	cdir $OF
	
	link $IF/config $OF/config
	vdirsyncer discover
	vdirsyncer sync
	vdirsyncer metasync
else
	nope vdirsyncer
fi

# khal
if check khal; then
	IF="$DOTFILES/khal"
	OF="$HOME/.config/khal"
	cdir $OF
	
	link $IF/config $OF/config
else
	nope khal
fi

# khard
if check khard; then
	IF="$DOTFILES/khard"
	OF="$HOME/.config/khard"
	cdir $OF

	link $IF/khard.conf $OF/khard.conf
else
	nope khard
fi

# networkmanager
if check networkmanager_dmenu; then
	IF="$DOTFILES/networkmanager"
	OF="$HOME/.config/networkmanager-dmenu"
	cdir $OF

	link $IF/config.ini $OF/config.ini
else
	nope networkmanager_dmenu
fi

# pubs
if check pubs; then
	if [ ! -d "$HOME/.local/share/pubs" ]; then
		pubs init -p $HOME/.local/share/pubs -d ~/.local/share/pubs/doc
		echo "Run \"git remote set-url origin\" in \"$HOME/.local/share/pubs\" to setup git"
	fi
	link $DOTFILES/pubs/pubsrc $HOME/.pubsrc
else
	nope pubs
fi

# pass
if check pass; then
	echo "Run \"git remote set-url origin\" in \"$HOME/.password-store\" to setup git"
else
	nope pass
fi

################################################################################
# Themes #######################################################################
################################################################################
while true; do
	read -p "Do you want to install ALL themes[y|n]" install_themes
	printf "\n"
	case $install_themes in
		[Yy]*)
			echo "Installing themes..."
			$HOME/dotfiles/setup/themes.sh
			break;;
		[Nn]*)
			echo "You can install themes later from $HOME/dotfiles/themes.sh"
			echo "Or install themes separately in $HOME/dotfiles/themes"
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
