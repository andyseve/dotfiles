" Author: Anish Sevekari
" Last Updated: Wed 16 Mar 2022 02:22:47 AM EDT

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

function! s:anish_theme_dict.solarized_dark() dict abort
	let g:solarized_termtrans=1
	let g:solarized_termcolors=16
	let g:solarized_contrast="normal"
	set background=dark
	colorscheme solarized
endfunction

function! s:anish_theme_dict.base16_solarized_dark() dict abort
	colorscheme base16-solarized-dark
endfunction

function! s:anish_theme_dict.base16_solarized_light() dict abort
	colorscheme base16-solarized-light
endfunction

function! s:anish_theme_dict.onedark() dict abort
	colorscheme onedark
endfunction

let s:anish_candidate_theme = ['solarized_dark', 'base16_solarized_dark', 'onedark']
let s:anish_theme = 'base16_solarized_dark'

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
