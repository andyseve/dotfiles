#!/bin/bash

THEMES=$HOME/.themes
THEMES_SOURCE=$HOME/dotfiles/themes
source $HOME/dotfiles/zsh/functions/helper.zsh

cdir $THEMES

for theme in $THEMES_SOURCE/*.sh; do
	read -n 1 -p "Installing $theme. Continue?[y|n]" cont
	printf "\n"
	if [ "$cont" = "y" ]; then
		$theme
	else
		continue
	fi
done
