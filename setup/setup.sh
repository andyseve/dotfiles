## Last Modified: Thu 26 Mar 2020 02:54:54 PM EDT
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

# bin
IF=$DOTFILES/bin
OF=$HOME/bin

cdir $OF

for file in "$IF"/*; do
	link $file $OF/${file##*/}
done

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
	link $IF/coc-settings.json $OF/coc-settings.json
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
	
	cdir $OF
	link $IF/init.vim $OF/init.vim
	link $DOTFILES/vim/coc-settings.json $OF/coc-settings.json
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
	OF="$HOME/.xmonad"
	cdir $OF

	link $IF/xmonad.hs $OF/xmonad.hs
	link $IF/xmobar.conf $OF/xmobar.conf
	
	FOLDERS=(lib scripts icons)
	for fol in ${FOLDERS[@]}; do
		link "$IF/$fol" "$OF/$fol"
	done
else
	nope xmonad
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
