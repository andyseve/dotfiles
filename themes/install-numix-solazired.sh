#!/bin/bash

sudo apt install numix-gtk-theme
sudo apt install ruby-sass libglib2.0-dev libgdk-pixbuf2.0-dev libxml2-utils
sudo apt install inkscape

source $HOME/dotfiles/zsh/functions/helper.zsh
OF=$HOME/.themes/numix-solarized-gtk-theme

clone git@github.com:andyseve/numix-solarized-gtk-theme.git $OF
cd $OF & sudo make install


echo "Installed Numix and NumixSolarized themes"
echo "To avoid dark theme popping up in firefix text fields, add
---- widget.content.gtk-theme-override = Numix ----
in firefox config"

