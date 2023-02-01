" Author: Anish Sevekari
" Last Updated: Wed 16 Nov 2022 05:02:11 AM EST
" tex specific vim settings

" Core Settings {{{
set spelllang=en_us
set spell
set textwidth=0

setlocal conceallevel=2

setlocal dictionary+=~/.config/nvim/spell/math.add
setlocal spellfile+=~/.config/nvim/spell/math.add

" }}}
" Key bindings and leader guide settings{{{
" This is where the fun starts.
" Make a spell check keybinding, to be ran at last

" }}}
" Autocorrects {{{
ab inv ^{-1}
ab teh the

" }}}
" Syntax Tweaks {{{
"hi! link texMathEnvArgName texEnvArgName
highlight! link Conceal SpecialChar
" }}}
" Functions {{{
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
	" might be better to write a python function instead of one in vim.
endfunction

" }}}
" Autocommands {{{
augroup anish_sevekari_tex
	autocmd!
	autocmd BufWinLeave *.* mkview
	autocmd BufWinEnter *.* silent loadview
augroup END

" }}}
" vim:foldmethod=marker:foldlevel=0:nospell
