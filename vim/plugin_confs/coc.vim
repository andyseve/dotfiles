" # COC Configuration
set signcolumn=yes " signcolumn is turned on to avoid toggle

" extensions
let g:coc_global_extensions = [
			\'coc-emoji',
			\'coc-dictionary',
			\'coc-tag',
			\'coc-snippets',
			\'coc-json',
			\'coc-html',
			\'coc-css',
			\'coc-python',
			\'coc-vimtex',
			\'coc-diagnostic'
			\]

" snippet mappings
map <c-l> <c-o>:CocList snippets<CR>
vmap <c-j> <Plug>(coc-snippets-select)
let g:coc_snippet_next = '<tab>'
let g:coc_snippet_prev = '<s-tab>'
xmap <leader>ex <Plug>(coc-convert-snippet)

" binding <tab> to select
" helper function to check if previous entry is a whitespace.
function! s:check_back_space() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1] =~ '\s'
endfunction

" Use tab to navigate manu and snippets
imap <silent><expr> <tab>
			\ coc#expandableOrJumpable() ? "\<c-r>=coc#_insert_key('request', 'snippets-expand-jump', 1)<cr>" :
			\ pumvisible() ? "\<c-n>":
			\ <SID>check_back_space() ? "\<tab>" :
			\ coc#refresh()

inoremap <silent><expr> <s-tab> pumvisible() ? "\<c-p>" : "\<c-h>"
inoremap <silent><expr> <c-space> coc#refresh()
inoremap <expr> <cr> pumvisible() ? coc#_select_confirm() : "\<c-g>u\<cr>"

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Use K to show documentation in preview window.
function! s:show_documentation()
	if (index(['vim','help'], &filetype) >= 0)
		execute 'h '.expand('<cword>')
	else
		call CocAction('doHover')
	endif
endfunction

nnoremap <silent> K :call <SID>show_documentation()<cr>
nnoremap <Plug>(coc-show-documentation) :call <SID>show_documentation()<cr>
nmap <silent> <leader>dk <Plug>(coc-show-documentation)

" GoTo code navigation.
nmap <silent> <leader>dd <Plug>(coc-definition)
nmap <silent> <leader>dy <Plug>(coc-type-definition)
nmap <silent> <leader>di <Plug>(coc-implementation)
nmap <silent> <leader>dr <Plug>(coc-references)

let g:lmap.d = {'name': 'GoTo'}

