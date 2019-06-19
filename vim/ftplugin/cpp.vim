" Author: Anish Sevekari
" Last Modified: Wed 19 Jun 2019 06:23:55 PM EDT
" Cpp specific vim settings

" Core Settings
	setlocal foldmethod=syntax

"	Tabstops
	setlocal tabstop=4
	setlocal softtabstop=4
	setlocal shiftwidth=4
	setlocal noexpandtab
	
" Mappings
	map <F5> :make main<CR>
	map <F6> :make run<CR>

" vim:foldmethod=marker:foldlevel=0:nospell
