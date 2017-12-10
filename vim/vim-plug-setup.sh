# !/bin/bash
# Last Updated:
# Author: Anish Sevekari

# This file handles the first time installation of vim-plug
if [ -e "~/.vim/autoload/plug.vim" ];
then
  echo "vim-plug already installed"
else
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
fi

