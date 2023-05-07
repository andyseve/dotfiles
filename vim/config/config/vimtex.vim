" # Vimtex configuration
let g:vimtex_enabled = 1
let g:vimtex_view_automatic = 1
let g:vimtex_toc_enabled = 1
let g:vimtex_toc_show_included_files = 1

" Latexmk Settings {{{
let g:vimtex_cache_root = '/tmp/vimtex'
let g:tex_flavor = "latex"
let g:vimtex_compiler_latexmk = {
			\ 'build_dir' : 'build',
			\ 'callback' : 1,
			\ 'continuous' : 1,
			\ 'executable' : 'latexmk',
			\ 'hooks' : [],
			\ 'options' : [
				\   '-shell-escape',
				\   '-verbose',
				\   '-file-line-error',
				\   '-synctex=1',
				\   '-interaction=nonstopmode',
				\ ],
				\}

" }}}
" Zathura config {{{
let g:vimtex_view_method = 'zathura'
let g:vimtex_view_general_viewer = 'zathura'

" }}}
" Table of contents {{{
let g:vimtex_toc_config = {
			\'name'           : 'LaTeX TOC',
			\'mode'           : 2,
			\'fold_enable'    : 1,
			\'show_help'      : 0,
			\'refresh_always' : 0
			\}

let g:vimtex_toc_config.layer_status = {
			\'content' : '1',
			\'label'   : '1',
			\'todo'    : '0',
			\'include' : '0'
			\}

let g:vimtex_toc_config.layer_keys = {
			\'content' : 'C',
			\'label'   : 'L',
			\'todo'    : 'T',
			\'include' : 'I'
			\}

" }}}
" Conceal {{{
let g:vimtex_syntax_conceal = {
			\ 'accents': 1,
			\ 'cites': 1,
			\ 'fancy': 1,
			\ 'greek': 1,
			\ 'math_bounds': 1,
			\ 'math_delimiters': 1,
			\ 'math_fracs': 0,
			\ 'math_super_sub': 0,
			\ 'math_symbols': 0,
			\ 'sections': 0,
			\ 'styles': 1,
			\}

let g:vimtex_syntax_conceal_cites = {
          \ 'type': 'brackets',
          \ 'icon': '📖',
          \ 'verbose': v:true,
          \}

" }}}


augroup VimTex
	autocmd!
	autocmd BufWritePost *.tex call vimtex#toc#refresh()
augroup END
