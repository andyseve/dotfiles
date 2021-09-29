" Author: Anish Sevekari
" Last Updated: Tue 28 Sep 2021 11:11:38 PM EDT

" Auto Commands

" No smartcase in command line mode https://vi.stackexchange.com/a/16511/15292 
augroup dynamic_smartcase
	autocmd!
	autocmd CmdLineEnter : set nosmartcase
	autocmd CmdLineLeave : set smartcase
augroup END

" More accurate syntax highlighting? (see `:h syn-sync`)
augroup accurate_syn_highlight
  autocmd!
  autocmd BufEnter * :syntax sync fromstart
augroup END

augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu | set rnu   | endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu | set nornu | endif
augroup END

" Auto-generate packer_compiled.lua file
augroup packer_auto_compile
  autocmd!
  autocmd BufWritePost */nvim/lua/plugins.lua source <afile> | PackerCompile
augroup END

" Custom commands
function! UpdateLastModified()
	" TODO: Delete the update search
	let b:cur_window = winsaveview()
	let b:last_line = line('$')
	if(b:last_line > 20)
		let b:last_line = 20
	endif
	let cmd = "0," . b:last_line . "s/Last \\(Modified\\|Edited\\|Changed\\|Updated\\): \\zs.*/\\=strftime(\"%c\")"
	execute cmd
	:nohlsearch
	call winrestview(b:cur_window)
endfunc

augroup anish_sevekari
	autocmd!
	" Update last modified tags at start of the file
	autocmd BufWritePre * silent! call UpdateLastModified() | redraw
	autocmd BufEnter * :nohlsearch
augroup END
