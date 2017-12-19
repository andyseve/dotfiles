" Maintained by Anish Sevekari
" Last Updated:

set nocompatible

" vim-plug {{{
  call plug#begin('~/.vim/bundle')
  Plug 'junegunn/vim-plug'
  Plug 'scrooloose/nerdtree'
  Plug 'Xuyuanp/nerdtree-git-plugin'
  Plug 'SirVer/ultisnips'
  Plug 'Valloric/YouCompleteMe', { 'do': './install.py --clang-completer --js-completer'}
  Plug 'lervag/vimtex', { 'for': 'latex' }
  Plug 'vim-syntastic/syntastic'
  Plug 'ctrlpvim/ctrlpvim'
  Plug 'sjl/gundo.vim'
  Plug 'tpope/vim-fugitive'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'jiangmiao/auto-pairs'

  Plug 'altercation/vim-colors-solarized'
  call plug#end()
" }}}
" Core Settings {{{
  syntax enable
  filetype plugin indent on
  if(has("gui_running"))
    " Set colorscheme to solarized dark
    set background=dark
    colorscheme solarized
    set guifont=ubuntu\ Mono\ 12
  else
    colorscheme desert
  endif
  set backspace=indent,eol,start
  set ruler " Display cursor position
  " Numbering Settings and functions {{{
    set number
    " Toggle between relative and absolute numbers
    function! RelativeNumber()
      set number
      set relativenumber
    endfunc
    function! AbsoluteNumber()
      set norelativenumber
      set number
    endfunc
    " Autocommands to toggle between relative and absolute numbering
    augroup AnishsNumber
      autocmd!
      autocmd FocusLost * call AbsoluteNumber()
      autocmd FocusGained * call RelativeNumber()
      autocmd BufLeave * call AbsoluteNumber()
      autocmd BufEnter * call RelativeNumber()
      autocmd InsertEnter * call AbsoluteNumber()
      autocmd InsertLeave * call RelativeNumber()
  " }}}
  set confirm " Exit confirmation
  set wildmenu
  set lazyredraw " Lazy redrawing of screen, force redraw using :redraw
  set showmatch " Highlight matching braces
  set modeline " Enables modlines (File specific vim settings)
  set modelines=1 " Read only the last line for vim specific options
  set autoread " Read changes from outside automatically
  set nomousehide " To fix a bug that causes cursor to dissapear
  set mouse=a
  set textwidth=0 " Textwidth to infinite
  set spell spelllang=en_us " Set US English for spellchecking
" }}}
" Tab settings {{{
  set autoindent
  set smartindent

  set shiftwidth=2
  set softtabstop=2
  set noexpandtab
" }}}
" Search settings {{{
  set hlsearch " Highlight searches
  set incsearch " Incremental search (As characters are entered)
  nnoremap <leader><space> :nohlsearch<CR> " Clear out search highlights
" }}}
" Fold Settings {{{
  set foldenable
  set foldlevelstart=3
" }}}
" Movements {{{
  nnoremap j gj
  nnoremap k gk
  vnoremap j gj
  vnoremap k gk
" }}}
" Bindings {{{
  nnoremap :W :w
  nnoremap :Q :q

  " Quick open files
  nnoremap <leader>ev :!gvim ~/.vimrc<CR>
  nnoremap <leader>eb :!gvim ~/.bashrc<CR> 
  nnoremap <leader>sv :source ~/.vimrc<CR>
  "nnoremap <leader>es :UltiSnipsEdit<CR>

  " Hotkeys for saving in gvim
  if(has("gui_running"))
    nnoremap <c-s> :update<CR>
    vnoremap <c-s> <c-c>:update<CR>
    inoremap <c-s> <c-o>:update<CR>
  endif

  " Global Copy and Paste
  nnoremap <leader>p "+gP
  vnoremap <leader>p "+gP
  nnoremap <leader>y "+y
  vnoremap <leader>y "+y

  " Backspace to delete words
  inoremap <c-backspace> <c-o>dF

" }}}

" vim:foldmethod=marker:foldlevel=0:nospell
