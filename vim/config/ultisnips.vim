" # Ultisnips Configuration  --Disabled in favor of COC                  

"if has("win64") || has("win32")
	"set rtp+=~/vimfiles
	"let g:UltiSnipsSnippetsDir="~/vimfiles/ultiSnips"
"else
	"set rtp+=~/.vim
	"let g:UltiSnipsSnippetsDir="~/.vim/ultiSnips"
"endif

let g:UltiSnipsEnableSnipMate = 0

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsListSnippets="<c-l>"

let g:UltiSnipsSnippetDirectories=["ultisnips"]
let g:UltiSnipsEditSplit="horizontal"

