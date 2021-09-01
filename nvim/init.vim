" Neovim Config file
" Author: Anish Sevekari
" Last Modified: Wed 01 Sep 2021 03:50:42 AM EDT


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


let g:config_plugin_list = [
			\'vim_leader.vim',
			\'coc.vim',
			\'vimtex.vim',
			\'gundo.vim',
			\'fzf_ag.vim',
			\'fugitive.vim',
			\'nerdtree.vim',
			\'nerd_commenter.vim',
			\'airline.vim'
			\]

for s:fname in g:config_plugin_list
	execute printf('source %s/config/%s', g:config_root, s:fname)
endfor

" vim:foldmethod=marker:syntax=vim

