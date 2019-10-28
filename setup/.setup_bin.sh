#!/bin/sh

OF=$HOME/bin
IF=$HOME/dotfiles/bin

if [ ! -x "$(command -v link)" ]; then
. $HOME/dotfiles/zsh/functions/helper.zsh
fi

for file in "$IF"/*; do
	link $file $OF/${file##*/}
done
