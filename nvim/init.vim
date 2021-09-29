" Neovim Config file
" Author: Anish Sevekari
<<<<<<< HEAD
" Last Modified: Thu 02 Sep 2021 04:57:21 PM EDT
=======
" Last Modified: Wed 08 Sep 2021 05:36:24 PM EDT
>>>>>>> 1e3ca44b09c2603c5b90df4112d1e71de36a6233


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

<<<<<<< HEAD
lua require("plugins")

let g:config_plugin_list = [
			\'airline.vim',
			\'fugitive.vim',
			\'nerd_commenter.vim',
			\'ultisnips.vim',
			\'vimtex.vim',
=======
lua require('plugins')


let g:config_plugin_list = [
			\'vimtex.vim',
			\'fzf_ag.vim',
			\'fugitive.vim',
			\'nerd_commenter.vim',
			\'airline.vim',
			\'ultisnips.vim'
>>>>>>> 1e3ca44b09c2603c5b90df4112d1e71de36a6233
			\]

for s:fname in g:config_plugin_list
	execute printf('source %s/config/%s', g:config_root, s:fname)
endfor

" vim:foldmethod=marker:syntax=vim

