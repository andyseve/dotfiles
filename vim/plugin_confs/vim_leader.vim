" # Vim Leader Guide Configuration                                       
nnoremap <silent> <leader> :<c-u>LeaderGuide '\'<CR>
vnoremap <silent> <leader> :<c-u>LeaderGuideVisual '\'<CR>
let g:lmap = {}
call leaderGuide#register_prefix_descriptions("\\", "g:lmap")

augroup AnishsVimLeader
	autocmd!
	autocmd FileType gitcommit noremap <buffer> <leader> <Plug>leaderguide-buffer
	autocmd BufEnter __Tagbar__ noremap <buffer> <leader> <Plug>leaderguide-buffer
	autocmd FileType NERDTreeType noremap <buffer> <leader> <Plug>leaderguide-buffer
augroup END

