" ## Solarized Config for Terminal
let g:solarized_termtrans=1
let g:solarized_termcolors=16
let g:solarized_contrast="normal"
set background=dark
colorscheme solarized

" ## FZF and Ag configuration
if executable('ag')
	set grepprg=ag\ --vimgrep
	let g:ackprg='ag --vimgrep'
	let g:ctrlp_user_command='ag --vimgrep -g'
endif
noremap <c-p> :FZF<CR>
