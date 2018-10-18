" Author: Anish Sevekari
" Last Modified: 10/17/2018 3:44:59 PM
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
