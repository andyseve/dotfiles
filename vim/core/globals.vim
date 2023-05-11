" Author: Anish Sevekari
" Last Updated: Wed 08 Mar 2023 01:37:29 AM EST

" vim global variables

" os detection
let g:is_win   = (has('win32') || has('win64'))    ? v:true : v:false
let g:is_linux = (has('unix')  || !has('macunix')) ? v:true : v:false
let g:is_mac   = has('macunix')                    ? v:true : v:false

" vim or nvim
let g:is_nvim  = has('nvim') ? v:true  : v:false
let g:is_vim   = has('nvim') ? v:false : v:true

" leader
let mapleader = "\\"
let g:lmap = {} " dictionary for vim-leader-guide
