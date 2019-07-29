" Author: Anish Sevekari
" Last Modified: Mon 29 Jul 2019 07:03:09 PM EDT
" Cpp specific vim settings

" # Core Settings
	setlocal foldmethod=manual

" # Tabstops
	setlocal tabstop=4
	setlocal softtabstop=4
	setlocal shiftwidth=4
	setlocal noexpandtab

" # Mappings
	nnoremap <F5> :silent !xsel -b > in<CR>
	nnoremap <F6> :Make main<CR>
	nnoremap <F7> :Make run<CR>
	nnoremap <F8> :Make debug<CR>

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
