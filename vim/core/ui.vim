" Author: Anish Sevekari
" Last Updated: Tue 15 Mar 2022 03:04:18 AM EDT

" UI and related settings

" # Setting visualbell for windeows                                         {{{
if g:is_win
	set guifont=Fira_Code:h10:cANSI:qDRAFT
	set renderoptions=type:directx
	set visualbell
else
	set guifont=Fira\ Code\ Regular\ 10
endif
"                                                                           }}}

" # Colorscheme setttings                                                   {{{
" Based on https://github.com/jdhao/nvim-config/blob/nvim-lsp/core/ui.vim 
let s:anish_theme_dict = {}

function! s:anish_theme_dict.solarized() dict abort
	let g:solarized_termtrans=1
	let g:solarized_termcolors=16
	let g:solarized_contrast="normal"
	set background=dark
	colorscheme solarized
endfunction

function! s:anish_theme_dict.onedark() dict abort
	colorscheme onedark
endfunction

function! s:anish_theme_dict.sonokai() dict abort
	let g:sonokai_enable_italics=1
	let g:sonokai_better_performance=1
	colorscheme sonokai
endfunction

let s:anish_candidate_theme = ['solarized', 'onedark', 'sonokai']
let s:anish_theme = 'onedark'

let s:anish_colorscheme_func = printf('s:anish_theme_dict.%s()', s:anish_theme)
if has_key(s:anish_theme_dict, s:anish_theme)
	execute 'call ' . s:anish_colorscheme_func
else
	if g:is_nvim
		call v:lua.vim.notify('Invalid colorscheme function: ' . s:anish_colorscheme_func, 'error', {'title': 'nvim-config'})
	else
		echoerr('Invalid colorscheme function: ' . s:anish_colorscheme_func)
	endif
endif
