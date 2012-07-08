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

"""""""""
" Theme "
"""""""""

syntax on
filetype plugin indent on
colorscheme Tomorrow-Night-Eighties
