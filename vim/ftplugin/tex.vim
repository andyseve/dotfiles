" Author: Anish Sevekari
" Last Updated: Tue 10 Sep 2019 04:43:22 PM EDT
" tex specific vim settings

" Core Settings {{{
	set spelllang=en_us
	set nospell
	set colorcolumn=100
" }}}
" Key bindings and leader guide settings{{{
	" This is where the fun starts.
	" Make a spell check keybinding, to be ran at last
	
" }}}
" autocorrects {{{
	ab inv ^{-1}
" }}}
" Ale config {{{
	let b:ale_max_signs = 20
	let b:ale_open_list = 0
" }}}
" functions {{{
	function! ImprortPreemble()
		" Preemble function.
		" Includes appropriate lines from anishs.sty into the latex file

		" Sanity check: Is the position outside begin document or not.
		" Import common packages (Can create a seperate micro for this)
		" Import function and operator definitions based on usage.
			" Maintain a list of commands which anishs.sty provides.
			" For each command, keep numbers of lines to import.
			" Exploit structure to automate this process.
			" Need not support more complicated commands!
			" Do a search over all the commands and import these commands.
			" autocmd to check for new commands in anishs.sty
	endfunction
" }}}
" vim:foldmethod=marker:foldlevel=0:nospell
