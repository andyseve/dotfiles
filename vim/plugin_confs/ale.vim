" ## ALE Configuration  --Disabled in favor of COC
set signcolumn=yes " signcolumn is turned on to avoid toggle
let g:ale_line_on_text_changed = 'never'
let g:ale_on_enter = 1
let g:ale_on_save = 1

if !exists('g:ale_linters')
	let g:ale_linters = {}
endif
let g:ale_linters.haskell = ['ghc-mod', 'hlint']


