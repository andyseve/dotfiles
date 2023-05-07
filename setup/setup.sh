## Last Modified: Wed 15 Feb 2023 10:16:36 PM EST
## This script creates all the symlinks from correct folders
## Based on similar script by Chris Cox

# Globals
CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}"
LOCAL="${XDG_DATA_HOME:-$HOME/.local/share}"
DOTFILES="$HOME/dotfiles"
OVERWRITE="$HOME/.overwrites"

DID_OVERWRITE=false
EXISTS_OVERWRITE=false
APPNAME=ALL
COLORSCHEME=
FORCE=false

# import helper functions
source $DOTFILES/zsh/functions/helper.zsh

# help function
help_function (){
	printf "This is help.\n"
}

# Overwrites
# FORCE variable checks if overwrites are to be forced or not. 
check_force() {
	if $FORCE; then 
		while true; do
			read -p "Running setup with --force. This will overwrite everything. Are you sure?" cont 
			printf "\n"
			case $cont in 
				[Yy]* ) break;;
				[Nn]* ) exit 0;;
				* ) printf "Invalid selection.\n";;
			esac
		done
	fi
}

show_overwrites() {
	if [ -e "$OVERWRITE" ]; then
		printf "Overwrite file already exists, with following contents:\n"
		if check bat; then bat $OVERWRITE; else cat $OVERWRITE; fi
		read -p "Continue?" cont
		while true; do
			case $cont in 
				[Yy]* ) break;;
				[Nn]* ) exit 0;;
				* ) printf "Invalid selection.\n";;
			esac
		done
		rm "$OVERWRITE"
	fi
	touch "$OVERWRITE"
}


# Application setups
# bash
bash_setup(){
if check bash; then
	link $DOTFILES/bash/bashrc $HOME/.bashrc
else
	nope bash
fi
}

# zsh
zsh_setup(){
if check zsh; then

	IF=$DOTFILES/zsh
	OF=${ZDOTDIR:-$HOME/.zsh}
	FOLDERS=(aliases functions)
	FILES=(.zshenv .zshrc p10k.zsh dircolors)

	cdir $OF

	for file in ${FILES[@]}; do
		link "$IF/$file" "$OF/$file"
	done

	for dir in ${FOLDERS[@]}; do
		link "$IF/$dir" "$OF/$dir"
	done

else
	nope zsh
fi
}

# git
git_setup() {
	if check git; then
		link $DOTFILES/git/gitconfig $HOME/.gitconfig
	fi
}

# alacritty
alacritty_setup() {
	if check alacritty; then
		IF=$DOTFILES/alacritty
		OF=$HOME/.config/alacritty
		link $IF/alacritty.yml $OF/alacritty.yml
	else
		nope alacritty
	fi
}

# vim
vim_setup() {
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
}

# nvim
nvim_setup() {
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
}

# neofetch
neofetch_setup() {
	if check neofetch; then
		cdir $CONFIG/neofetch
		link $DOTFILES/neofetch/config.conf $CONFIG/neofetch/config.conf
	else
		nope neofetch
	fi
}

#latex
latex_setup() {
	if check latex; then
		cdir $HOME/texmf/tex/latex/local
		link $DOTFILES/latex/anishs.sty $HOME/texmf/tex/latex/local/anishs.sty
	fi
}

# rtorrent
rtorrent_setup() {
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
}

# xmonad
xmonad_setup() {
	if check xmonad; then
		IF="$DOTFILES/xmonad"
		OF="$CONFIG/xmonad"
		cdir $OF

		link $IF/xmonad.hs $OF/xmonad.hs
		link $IF/scripts $OF/scripts
	else
		nope xmonad
	fi
}

# xmobar
xmobar_setup() {
	if check xmobar; then
		IF="$DOTFILES/xmobar"
		OF="$CONFIG/xmobar"
		cdir $OF
		
		if [ ! -L $OF/xmobar.hs ]; then
			echo "Choose the primary xmobar file to link"
			select opt in $IF/*.hs
			do
				link $opt $OF/xmobar.hs
				break
			done
		fi
		# link $IF/my-xmobar.cabal $OF/my-xmobar.cabal
		# link $IF/shell.nix $OF/shell.nix
		# link $IF/hie.yaml $OF/hie.yaml
		link $IF/xmobar-secondary.hs $OF/xmobar-secondary.hs
		link $IF/scripts $OF/scripts
	else
		nope xmobar
	fi
}

# xfce_terminal
xfce_terminal_setup() {
	if check xfce4-terminal; then
		cdir $HOME/.config/xfce4/terminal
		link $DOTFILES/xfce4/terminal/terminalrc $HOME/.config/xfce4/terminal/terminalrc
	else
		nope xfce4-terminal
	fi
}

# ranger
ranger_setup() {
	if check ranger; then
		IF="$DOTFILES/ranger"
		OF="$HOME/.config/ranger"
		cdir $OF
		
		link $IF/rc.conf $OF/rc.conf
		link $IF/commands.py $OF/commands.py
	else
		nope ranger
	fi
}

# rofi
rofi_setup() {
	if check rofi; then
		IF="$DOTFILES/rofi"
		OF="$HOME/.config/rofi"
		cdir $OF

		link $IF/config.rasi $OF/config.rasi
	else
		nope rofi
	fi
}

# zathura
zathura_setup() {
	if check zathura; then
		IF="$DOTFILES/zathura"
		OF="$HOME/.config/zathura"
		cdir $OF

		link $IF/zathurarc $OF/zathurarc
	else
		nope zathura
	fi
}

# dunst
dunst_setup() {
	if check dunst; then
		IF="$DOTFILES/dunst"
		OF="$HOME/.config/dunst"
		cdir $OF

		link $IF/dunstrc $OF/dunstrc
	else
		nope dunst
	fi
}

# picom
picom_setup() {
	if check picom; then
		IF="$DOTFILES/picom"
		OF="$HOME/.config/picom"
		cdir $OF

		link $IF/picom.conf $OF/picom.conf
	else
		nope picom
	fi
}

# vdirsyncer
vdirsyncer_setup() {
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
}

# khal
khal_setup() {
	if check khal; then
		IF="$DOTFILES/khal"
		OF="$HOME/.config/khal"
		cdir $OF
		
		link $IF/config $OF/config
	else
		nope khal
	fi
}

# khard
khard_setup() {
	if check khard; then
		IF="$DOTFILES/khard"
		OF="$HOME/.config/khard"
		cdir $OF

		link $IF/khard.conf $OF/khard.conf
	else
		nope khard
	fi
}

# networkmanager
networkmanager_setup() {
	if check networkmanager_dmenu; then
		IF="$DOTFILES/networkmanager"
		OF="$HOME/.config/networkmanager-dmenu"
		cdir $OF

		link $IF/config.ini $OF/config.ini
	else
		nope networkmanager_dmenu
	fi
}

# pubs
pubs_setup() {
	if check pubs; then
		if [ ! -d "$HOME/.local/share/pubs" ]; then
			pubs init -p $HOME/.local/share/pubs -d ~/.local/share/pubs/doc
			echo "Run \"git remote set-url origin\" in \"$HOME/.local/share/pubs\" to setup git"
		fi
		link $DOTFILES/pubs/pubsrc $HOME/.pubsrc
	else
		nope pubs
	fi
}

# pass
pass_setup() {
	if check pass; then
		echo "Run \"git remote set-url origin\" in \"$HOME/.password-store\" to setup git"
	else
		nope pass
	fi
}


# Checking if script is sources or executed
IS_SOURCED=true
if [[ -n $ZSH_VERSION && $ZSH_EVAL_CONTEXT =~ :file$ ]] ; then IS_SOURCED=true;
# elif [[ -n $BASH_VERSION && (return 0 2>/dev/null) ]]; then IS_SOURCED=false;
else IS_SOURCED=false;
fi

if ! $IS_SOURCED; then
	# Argument processing
	OPTS=$(getopt -o hfa:c: --long help_function,force,appname:,color: -n 'setup' -- "$1")
	if [ $? != 0 ] ; then printf "Incorrect arguments...\n"; help_function ; exit 1; fi

	eval set -- "$OPTS"

	while true; do
		case "$1" in 
			-h | --help ) help_function; exit 0;;
			-f | --force ) FORCE=true; shift;;
			-a | --appname ) APPNAME=$2; shift 2;;
			-c | --color ) COLORSCHEME=$2; shift 2;;
			-- ) shift; break;;
			* ) help_function; exit 1;;
		esac
	done

	check_force
	show_overwrites
fi

