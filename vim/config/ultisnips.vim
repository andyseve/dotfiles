# Ultisnips Configuration  --Disabled in favor of COC                  
if has("win64") || has("win32")
	set rtp+=~/vimfiles
	let g:UltiSnipsSnippetsDir="~/vimfiles/UltiSnips"
else
	set rtp+=~/.vim
	let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"
endif
let g:UltiSnipsExpandTrigger="<c-space>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsListSnippets="<c-l>"

let g:UltiSnipsSnippetsDirectoris=['UltiSnips']
let g:UltiSnipsEditSplit="horizontal"

