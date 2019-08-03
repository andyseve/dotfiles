" Author: Anish Sevekari
" Last Modified: Sat 03 Aug 2019 11:40:19 AM EDT
" Cpp specific vim settings

" # Core Settings
	setlocal foldmethod=manual

" # Tabstops
	setlocal tabstop=4
	setlocal softtabstop=4
	setlocal shiftwidth=4
	setlocal noexpandtab

" # Mappings
	function! Update_In()
		silent !xsel -b > in
		redraw!
	endfunction
	nnoremap <F5> :call Update_In()<CR>
	nnoremap <F6> :Make main<CR>
	nnoremap <F7> :Make run<CR>
	nnoremap <F8> :Make debug<CR>

" # ALE Settings
	let b:ale_linters = ['gcc', 'clang']
	let b:ale_fixers = ['clang-format', 'uncrustify']
	let b:ale_cpp_gcc_options = '-std=c++17 -Wall -O3 -pthread -lm'

" # Abbreviations
<<<<<<< HEAD
	ab ll long long
	ab pii pair<int,int>
	ab vi vector<int>
	ab vll vector<long long>
	ab vpii vector<pair<int,int>>
	ab mi modular<int>
	ab pb push_back(
	ab mp make_pair(
=======
	iab ll long long
	iab pii pair<int,int>
	iab vi vector<int>
	iab vll vector<long long>
	iab vpii vector<pair<int,int>>
	iab mi modular<int>
	iab pb push_back(
	iab mp make_pair(
>>>>>>> 6d02fa74134603d508582dc8e182e8eef7b41b24
"  vim:foldmethod=marker:foldlevel=0:nospell
