" # FZF and Ag configuration
if executable('ag')
	set grepprg=ag\ --vimgrep
	let g:ackprg='ag --vimgrep'
	let g:ctrlp_user_command='ag --vimgrep -g'
endif
noremap <c-p> :FZF<CR>
