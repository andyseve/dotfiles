" # Neovim Config Init file
" Author: Anish Sevekari
" Last Updated: Sat 04 Jun 2022 02:59:00 AM EDT

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
lua require('core.mappings')

" vim:foldmethod=marker:syntax=vim

