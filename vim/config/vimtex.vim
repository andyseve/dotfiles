" # Vimtex configuration
let g:vimtex_enabled = 1
let g:vimtex_view_automatic = 1
let g:vimtex_toc_enabled = 1
let g:vimtex_toc_depth = 1
let g:vimtex_toc_show_included_files = 1
let g:vimtex_toc_fold = 1

let g:vimtex_cache_root = '/tmp/vimtex'

" LeaderGuide
let g:lmap.l = {
			\'name': 'Latex',
			\}

" SumatraPDF config for windows
if has("win64") || has("win32")
	let g:vimtex_view_general_viewer = 'SumatraPDF.exe'
	let g:vimtex_view_general_options
				\ = ' -forward-search @tex @line @pdf'
				\ . ' -inverse-search "gvim.exe --servername ' . v:servername
				\ . ' --remote-send \"^<C-\^>^<C-n^>'
				\ . ':drop \%f^<CR^>:\%l^<CR^>:normal\! zzzv^<CR^>'
				\ . ':execute ''drop '' . fnameescape(''\%f'')^<CR^>'
				\ . ':\%l^<CR^>:normal\! zzzv^<CR^>'
				\ . ':call remote_foreground('''.v:servername.''')^<CR^>^<CR^>\""'
else
	let g:vimtex_view_general_viewer = 'zathura'
	let g:vimtex_view_method = 'zathura'
endif

if has("nvim")
	let g:vimtex_compiler_progname = 'nvr'
endif
