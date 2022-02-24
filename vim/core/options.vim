" Vim Core Options
" Author: Anish Sevekari
" Last Updated: Thu 17 Feb 2022 04:52:39 PM EST

" Options

set encoding=utf-8

" # Symtax Settings                                                         {{{
syntax enable
set synmaxcol=0 " Maximum syntax coloring column

"                                                                           }}}
" # Indent Settings                                                         {{{
filetype plugin indent on
set autoindent
set smartindent
set breakindent
set showbreak=↳\ 

set shiftwidth=2
set softtabstop=2
set tabstop=2
set noexpandtab

"                                                                           }}}
" # General Settings                                                        {{{
set backspace=indent,eol,start " Backspace settings
set ruler                      " Display cursor position
set number                     " Number lines
set confirm                    " Exit confirmation
set wildmenu
set wildmode=full
set lazyredraw                 " Lazy redrawing of screen, force redraw using :redraw
set showcmd                    " Show (partial) command in status line
set showmatch                  " Highlight matching braces
set modeline                   " Enables modlines (File specific vim settings)
set modelines=1                " Read only the last line for vim specific options
set autoread                   " Read changes from outside automatically
set autowrite                  " Automatically save before commands like :next and :make
set nomousehide                " To fix a bug that causes cursor to dissapear
set mouse=a                    " Enable mouse
set textwidth=0                " Textwidth to infinite
set spelllang=en_us            " Set US English for spellchecking
set nospell
set viewoptions-=options       " Excludes options (like working dir) from view
set updatetime=300
set hidden                     " Hide buffers when they are abandoned
set cmdheight=2                " Setting command height 2 for more space
set shortmess+=c               " Messages should be short
set whichwrap+=<,>,h,l,[,]     " Wraping around the lines
set splitbelow splitright      " Window split settings
set completeopt=menu,menuone,noselect
set termguicolors              " Terminal Color settings

" Ignore certain files and folders when globing
set wildignore+=*.o,*.obj,*.bin,*.dll,*.exe
set wildignore+=*/.git/*,*/.svn/*,*/__pycache__/*,*/build/**
set wildignore+=*.jpg,*.png,*.jpeg,*.bmp,*.gif,*.tiff,*.svg,*.ico
set wildignore+=*.pyc
set wildignore+=*.DS_Store
set wildignore+=*.aux,*.bbl,*.blg,*.brf,*.fls,*.fdb_latexmk,*.synctex.gz,*.xdv
set wildignorecase  " ignore file and dir name cases in cmd-completion

" Completion
set complete+=kspell

"                                                                           }}}
" # Gui Settings                                                            {{{
if has('gui_running')
	set guioptions-=L " Remove left scrollbar
	set guioptions-=T " Remove Toolbar
	set guioptions-=m " Remove Menubar
endif

"                                                                           }}}
" # Search Settings                                                         {{{
set hlsearch   " Highlight searches
set incsearch  " Incremental search (As characters are entered)
set ignorecase " Do case insensitive matching
set smartcase  " Do smart case matching

"                                                                           }}}
" # Fold Settings                                                           {{{
set foldenable
set foldlevel=0
set foldmethod=marker

"                                                                           }}}
" # Back up directories                                                     {{{
if g:is_nvim
	set undodir=~/.local/share/nvim/undo
	set backupdir=~/.local/share/nvim/backup
	set directory=~/.local/share/nvim/swp
	set viewdir=~/.local/share/nvim/view
elseif g:is_win
	set undodir=~/vimfiles/.undo
	set backupdir=~/vimfiles/.backup
	set directory=~/vimfiles/.swp
	set viewdir=~/vimfiles/.view
else
	set undodir=~/.vim/.undo
	set backupdir=~/.vim/.backup
	set directory=~/.vim/.swp
	set viewdir=~/.vim/.view
endif

"                                                                           }}}

