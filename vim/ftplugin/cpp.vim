" Author: Anish Sevekari
" Last Modified: Thu 27 Oct 2022 03:05:25 AM EDT
" Cpp specific vim settings

" # Core Settings
	setlocal foldmethod=expr
	setlocal foldexpr=nvim_treesitter#foldexpr()
	setlocal foldminlines=5
	setlocal foldlevelstart=99

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

" # Abbreviations
	ab vi vector<int>
	ab vvi vector<vector<int>>
	ab vm vector<modular>
	ab lll ll
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

" # Autocommands

"  vim:foldmethod=marker:foldlevel=0:nospell
