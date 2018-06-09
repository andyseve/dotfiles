" Author: Anish Sevekari
" Last Modified: Wed 09 May 2018 09:41:14 AM EDT
" Cpp specific vim settings

" Core Settings {{{
	setlocal foldmethod=syntax
	setlocal foldnestmax=3
" }}}
" Tabstops {{{
	set tabstop=2
	set softtabstop=2
	set shiftwidth=2
	set noexpandtab
" }}}
" Mappings {{{
	map <F5> :make main<CR>
	map <F6> :make run<CR>
" }}}

" vim:foldmethod=marker:foldlevel=0:nospell
