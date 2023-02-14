" Author: Anish Sevekari
" Last Modified: Wed 19 Jun 2019 06:36:11 PM EDT
" zsh syntax fix

" Syntax fix
	source $VIMRUNTIME/syntax/zsh.vim
	unlet b:current_syntax
	source $VIMRUNTIME/syntax/sh.vim
	let b:current_syntax = "zsh"

