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

" Powerline
set laststatus=2
set t_Co=256

" Remember last cursor position
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
