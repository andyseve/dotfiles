# !bin/bash
# Last Updated:
# Author: Anish Sevekari

# This file handles first time vim setup to the dotfiles,
# including symlink setup and vim-plug setup
# and also installs vim 8

# vim 8 setup, need root access for this
sudo add-apt-repository ppa:jonathonf/vim
sudo apt update
sudo apt upgrade
sudo apt install vim

ln -s . ~/.vim
ln -s ../vimrc ~/.vimrc

./vim-plug-setup.sh
