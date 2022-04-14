" # Neovim Config Init file
" Author: Anish Sevekari
" Last Updated: Thu 03 Mar 2022 01:53:22 PM EST

let g:config_root = fnamemodify(resolve(expand('<sfile>:p')), ':h')

let g:config_file_list = [
			\'globals.vim',
			\'options.vim',
			\'autocommands.vim',
			\'utils.vim',
			\'ui.vim',
			\'keys.vim'
			\]

for s:fname in g:config_file_list
	execute printf('source %s/core/%s', g:config_root, s:fname)
endfor

lua require('plugins')

" vim:foldmethod=marker:syntax=vim

