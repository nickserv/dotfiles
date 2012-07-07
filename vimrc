" PATHOGEN
runtime bundle/pathogen/autoload/pathogen.vim
let g:pathogen_disabled = []
call pathogen#infect()
call pathogen#helptags()

" POWERLINE
set laststatus=2
set t_Co=256

" THEME
syntax on
filetype plugin indent on
colorscheme Tomorrow-Night-Eighties
