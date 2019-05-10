" Author: Anish Sevekari
" Last Modified: Thu 02 May 2019 06:35:35 PM EDT
" Cpp specific vim settings

" Core Settings {{{
	setlocal foldmethod=syntax
	setlocal foldnestmax=3
" }}}
" Tabstops {{{
	set tabstop=4
	set softtabstop=4
	set shiftwidth=4
	set noexpandtab
" }}}
" Mappings {{{
	map <F5> :make main<CR>
	map <F6> :make run<CR>
" }}}

" vim:foldmethod=marker:foldlevel=0:nospell
