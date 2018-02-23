# !bin/bash
# Last Updated: Tue 20 Feb 2018 05:29:14 PM EST
# Author: Anish Sevekari

# This file handles first time vim setup to the dotfiles,
# like setting up symlinks and installing vim-plug

# External vim8 ppa not needed.

ln -s ./vim ~/.vim
ln -s ./vimrc ~/.vimrc
ln -s ./ycm_extra_conf.py ~/.ycm_extra_conf.py

./vim-plug-setup.sh

