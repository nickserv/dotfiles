""""""""""""
" Pathogen "
""""""""""""

runtime bundle/pathogen/autoload/pathogen.vim
let g:pathogen_disabled = []
call pathogen#infect()
call pathogen#helptags()

""""""
" UI "
""""""

" disable vi features
set nocompatible

" mouse support
set mouse=a

" line numbers
set number

" highlight cursor line
set cursorline

" search stuff
set hlsearch
set incsearch
set ignorecase
set smartcase
set gdefault

" automatically load changed files
set autoread

" Powerline
set laststatus=2
set t_Co=256

" remember last cursor position
autocmd BufReadPost *
	\ if line("'\"") > 0 && line("'\"") <= line("$") |
	\ 	exe "normal g`\"" |
	\ endif

"""""""""
" Theme "
"""""""""

syntax on
filetype plugin indent on
colorscheme Tomorrow-Night-Eighties
