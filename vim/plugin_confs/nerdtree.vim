" # NerdTree Configuration
let g:NERDTreeQuitOnOpen = 1
nnoremap <M-\> :NERDTreeToggle<CR>
inoremap <M-\> <c-o>:NERDTreeToggle<CR>
" Close if only windows present is NERDTree
augroup AnishsNerdTree
	autocmd!
	autocmd BufEnter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
augroup END
