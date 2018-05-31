#!/bin/bash
# Last Updated: Wed 30 May 2018 09:43:23 PM EDT
# Setup script for setting up zsh
# Easier way would be to ofcourse use oh-my-zsh.


ln -s /home/stranger/dotfiles/zsh/zshrc ~/.zshrc
mkdir ~/.zprompts
ln -s /home/stranger/dotfiles/zsh/prompts/agnoster/agnoster.zsh-theme ~/.zpromts/prompt_agnoster_setup
