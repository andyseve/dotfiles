" Author: Anish Sevekari
" Last Modified: Tue 13 Aug 2019 01:09:06 AM EDT
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
	let b:ale_cpp_gcc_options = '-std=c++17 -Wall -Wno-unused-result -O3 -pthread -lm'

" # Abbreviations
	iab ll long long
	iab pii pair<int,int>
	iab vi vector<int>
	iab vll vector<long long>
	iab vpii vector<pair<int,int>>
	iab mi modular<int>
	ab pb push_back(
	ab mp make_pair(
	ab cendl cout << endl;
	ab newline cout << "\n";
	ab *=2 <<= 1
"  vim:foldmethod=marker:foldlevel=0:nospell
