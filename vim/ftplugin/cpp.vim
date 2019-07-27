" Author: Anish Sevekari
" Last Modified: Wed 24 Jul 2019 05:52:20 PM EDT
" Cpp specific vim settings

" # Core Settings
	setlocal foldmethod=manual

" # Tabstops
	setlocal tabstop=4
	setlocal softtabstop=4
	setlocal shiftwidth=4
	setlocal noexpandtab

" # Mappings
	nnoremap <F5> :silent! !{xsel -b > in} \| redraw!<CR>
	nnoremap <F6> :Make main<CR>
	nnoremap <F7> :Make run<CR>

" # ALE Settings
	let b:ale_linters = ['gcc']
	let b:ale_fixers = ['clang-format', 'uncrustify']
	let b:ale_cpp_gcc_options = '-std=c++17 -Wall -O3 -pthread -lm'

" vim:foldmethod=marker:foldlevel=0:nospell
