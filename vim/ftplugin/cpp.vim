" Author: Anish Sevekari
" Last Modified: Thu 19 Nov 2020 09:39:26 PM EST
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
	ab vi vector<int>
	ab vm vector<modular>
	ab ll long long
	ab 1ll 1ll
	ab vll vector<long long>
	ab pii pair<int,int>
	ab vpii vector<pair<int,int>>
	ab cd complex<double>
	ab vcd vector<complex<double>>
	ab pb push_back(
	ab mp make_pair(
	ab cendl cout << endl
	ab newline cout << "\n"
	ab *=2 <<= 1
"  vim:foldmethod=marker:foldlevel=0:nospell
