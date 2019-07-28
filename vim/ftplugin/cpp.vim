" Author: Anish Sevekari
" Last Modified: Sun 28 Jul 2019 03:45:09 AM EDT
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
	let b:ale_linters = ['gcc', 'clang']
	let b:ale_fixers = ['clang-format', 'uncrustify']
	let b:ale_cpp_gcc_options = '-std=c++17 -Wall -O3 -pthread -lm'

" # Abbreviations
	ab ll long long
	ab pii pair<int,int>
	ab vi vector<int>
	ab vll vector<long long>
	ab vpii vector<pair<int,int>>
	ab mi modular<int>
	ab pb push_back(
	ab mp make_pair(
"  vim:foldmethod=marker:foldlevel=0:nospell
