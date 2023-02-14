" # YouCompleteMe Configuration
set signcolumn=yes " signcolumn is turned on to avoid toggle
if has("win64") || has("win32")
	let g:ycm_server_python_interpreter = 'python'
else
	let g:ycm_server_python_interpreter = 'python3'
endif

let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

let g:ycm_global_extra_conf='~/.ycm_extra_conf.py'
let g:ycm_global_ycm_extra_conf='~/.ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 0

let g:ycm_collect_identifiers_from_tag_files = 1
let g:ycm_seed_identifier_with_syntax = 1
let g:ycm_complete_in_comments = 1
let g:ycm_cache_omnifunc = 1

let g:ycm_show_diagnostic_ui = 0

if !exists('g:ycm_sematics_triggers')
	let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers.cpp = ['re!(?=[a-zA-Z0-9_]{3})']
let g:ycm_semantic_triggers.tex = g:vimtex#re#youcompleteme

