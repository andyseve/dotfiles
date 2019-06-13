" Author: Anish Sevekari
" Last Modified: Sun 19 May 2019 04:42:15 PM EDT
" Cpp specific vim settings

" Core Settings {{{
	setlocal foldmethod=syntax
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
