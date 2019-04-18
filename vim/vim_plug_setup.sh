# !/bin/bash
# Last Updated:
# Author: Anish Sevekari

# This file handles the first time installation of vim-plug
if [ -e "$1" ];
then
  echo "vim-plug already installed"
else
  curl -fLo "$1" --create-dirs "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
fi

