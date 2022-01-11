" # Neovim Config Init file
" Author: Anish Sevekari
" Last Updated: Mon 10 Jan 2022 06:04:32 AM EST

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


let g:config_plugin_list = [
			\'vimtex.vim',
			\'fzf_ag.vim',
			\'fugitive.vim',
			\'nerd_commenter.vim',
			\'airline.vim',
			\'ultisnips.vim'
			\]

for s:fname in g:config_plugin_list
	execute printf('source %s/config/%s', g:config_root, s:fname)
endfor

" vim:foldmethod=marker:syntax=vim

