" Author: Anish Sevekari
" Last Modified: Mon 20 May 2019 08:58:10 PM EDT
" Cpp specific vim settings

" # Core Settings
	setlocal foldmethod=syntax

" # Tabstops
	setlocal tabstop=4
	setlocal softtabstop=4
	setlocal shiftwidth=4
	setlocal noexpandtab

" # Mappings
	map <F5> :make main<CR>
	map <F6> :make run<CR>

" # ALE Settings
	let b:ale_linters = ['gcc']
	let b:ale_fixers = ['clang-format', 'uncrustify']
	let b:ale_cpp_gcc_options = '-std=c++17 -Wall -O3 -pthread -lm'

" vim:foldmethod=marker:foldlevel=0:nospell
