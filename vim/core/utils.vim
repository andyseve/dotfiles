" Author: Anish Sevekari
" Last Updated: Sun 15 Aug 2021 02:19:17 AM EDT

" Utility functions

" CP environment
function! utils#code() abort
	:cd ~/Code
	:e main.cpp
	:vs out
	:split in
	:vertical resize 40
endfunc

" Remove trailing white space, see https://vi.stackexchange.com/a/456/15292
function! utils#StripTrailingWhitespaces() abort
  let l:save = winsaveview()
  " vint: next-line -ProhibitCommandRelyOnUser -ProhibitCommandWithUnintendedSideEffect
  keeppatterns %s/\v\s+$//e
  call winrestview(l:save)
endfunction

" Create command alias safely, see https://stackoverflow.com/q/3878692/6064933
" The following two functions are taken from answer below on SO:
" https://stackoverflow.com/a/10708687/6064933
function! utils#Cabbrev(key, value) abort
  execute printf('cabbrev <expr> %s (getcmdtype() == ":" && getcmdpos() <= %d) ? %s : %s',
        \ a:key, 1+len(a:key), <SID>Single_quote(a:value), <SID>Single_quote(a:key))
endfunction
