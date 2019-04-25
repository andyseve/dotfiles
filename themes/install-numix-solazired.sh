#!/bin/bash

sudo apt install numix-gtk-theme
sudo apt install ruby-sass libglib2.0-dev libgdk-pixbuf2.0-dev libxml2-utils
sudo apt install inkscape

THEME=$HOME/dotfiles/themes/numix-solarized-gtk-theme

cd $THEME && sudo make install

