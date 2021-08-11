" ## Airline Configuration
let g:airline_theme = 'solarized'
let g:airline_solarized_bg = 'dark'
let g:airline_powerline_fonts = 1
let g:airline_inactive_collapse = 1
let g:airline_skip_empty_sections = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#tabline#enabled = 1

" Airline symbols
if !exists('g:airline_symbols')
	let g:airline_symbols ={}
endif
let g:airline_symbols.space = "\u3000"
if has("win64") || has("win32")
	let g:airline_powerline_fonts = 0
	let g:airline_symbols.space = "\u0020"
endif

