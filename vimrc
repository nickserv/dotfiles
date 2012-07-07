" PATHOGEN
runtime bundle/pathogen/autoload/pathogen.vim
let g:pathogen_disabled = []
call pathogen#infect()
call pathogen#helptags()

" THEME
syntax on
filetype plugin indent on
colorscheme Tomorrow-Night-Eighties
