" Author: Anish Sevekari
" Last Updated: Fri 27 May 2022 07:24:51 AM EDT

" Keybindings
" Has vim-global leader settings: its better to bind keys outside the function
" too.
nnoremap :W :w
nnoremap :Q :q
nnoremap -d dd
nnoremap -c ddO

" Movements
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Hotkeys for saving in gvim
if(has("gui_running"))
	nnoremap <c-s> :update<CR>
	vnoremap <c-s> <c-c>:update<CR>
	inoremap <c-s> <c-o>:update<CR>
endif

" Reload vimrc
nnoremap sv :source $MYVIMRC<CR>

" Backspace to delete words
imap <c-backspace> <c-o>:normal dbx<CR>
nmap <c-backspace> dbx

" Copy and paste from system clipboard
nnoremap <leader>p "+gP
nnoremap <leader>y "+y
let g:lmap.p = ['call feedkeys("\"+gP")', 'Paste']
let g:lmap.y = ['call feedkeys("\"+y")', 'Copy']

" Remove search highlights
nmap <leader>h :nohlsearch<CR>
let g:lmap.h = 'Highlight'

" Quick lists
nmap <leader>ok :copen<CR>
nmap <leader>oo :cwindow<CR>
nmap <leader>ol :lwindow<CR>

let g:lmap.o = {
			\'name': 'Lists'
			\}
let g:lmap.o.k = ['copen', 'Quickfix']
let g:lmap.o.o = ['cwindow', 'Quickfix']
let g:lmap.o.l = ['lwindow', 'Locationlist']

" Quick Edits
nmap <leader>es :CocCommand snippets.editSnippets<CR>
nmap <leader>ev :vsplit ~/.vimrc<CR>
nmap <leader>en :vsplit ~/.config/nvim/init.vim<CR>
nmap <leader>el :vsplit ~/dotfiles/latex/anishs.sty<CR>
nmap <leader>ez :vsplit ~/dotfiles/zsh/zshrc<CR>

let g:lmap.e = {
			\'name': 'Quick Edit',
			\'s':['CocCommand snippets.editSnippets', 'Snippets'],
			\'v':['vsplit ~/.vimrc', 'Vimrc'],
			\'n':['vsplit ~/.config/nvim/init.vim', 'Neovim'],
			\'l':['vsplit ~/dotfiles/latex/anishs.sty', 'Latex'],
			\'z':['vsplit ~/dotfiles/zsh/zshrc', 'Zsh'],
			\}

" Move keymaps from this file into setup files
" This allows functions to be loaded, so that keymaps work before functions
" have been loaded
" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

