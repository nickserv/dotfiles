""""""""""
" Vundle "
""""""""""

set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'tpope/vim-pathogen'
Bundle 'chriskempson/vim-tomorrow-theme'
Bundle 'Lokaltog/vim-powerline'
Bundle 'tpope/vim-fugitive'
Bundle 'taglist.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-unimpaired'
Bundle 'kien/ctrlp.vim'
Bundle 'Townk/vim-autoclose'
Bundle 'matchit.zip'
Bundle 'mattn/zencoding-vim'
Bundle 'tpope/vim-git'
Bundle 'jceb/vim-orgmode'
Bundle 'mileszs/ack.vim'

" recommended by Vundle
"Bundle 'tpope/vim-fugitive'
"Bundle 'Lokaltog/vim-easymotion'
"Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
"Bundle 'tpope/vim-rails.git'
"Bundle 'L9'
"Bundle 'FuzzyFinder'
"Bundle 'git://git.wincent.com/command-t.git'

filetype plugin indent on     " required!
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..

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

" wrapping stuff
set textwidth=80
set colorcolumn=80

" ignore whitespace in diff mode
set diffopt+=iwhite

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

" show '>   ' at the beginning of lines that are automatically wrapped
set showbreak=>\ \ \ 

"""""""""""""
" Indenting "
"""""""""""""

set tabstop=2 "width (in spaces) that a <tab> is displayed as
set shiftwidth=2 "width (in spaces) used in each step of autoindent (aswell as << and >>)
set softtabstop=2 "control how many columns vim uses when you hit Tab in insert mode
set noexpandtab "use real tabs, not spaces, for indents
set copyindent "when auto-indenting, use the indenting format of the previous line
set smarttab "shiftwidth used in front of a line, but tabstop used otherwise
set autoindent "turn on auto-indenting (great for programers)
set smartindent "automatically inserts one extra level of indentation in some cases, and works for C-like files

"""""""""
" Theme "
"""""""""

syntax on
filetype plugin indent on
colorscheme Tomorrow-Night-Bright

""""""""
" GVim "
""""""""

set gfn=Tamsyn\ 8 "set font
set gcr=n:blinkon0 "disable cursor blinking

set guioptions-=m "remove menu bar
set guioptions-=T "remove toolbar
set guioptions-=L "remove left scroll bar
set guioptions-=r "remove right scroll bar

""""""""""""
" Mappings "
""""""""""""

" change leader from \ to ,
let mapleader=","

" enter vim commands without having to press shift
map ; :

" typing ;; quickly exits insert mode
imap ;; <Esc>

" bubble text up and down
nmap <Up>   [e
imap <Up>   <C-O><Up>
vmap <Up>   [egv
nmap <Down> ]e
imap <Down> <C-O><Down>
vmap <Down> ]egv

" indent text left and right
nmap <Left>   <<
imap <Left>   <C-O><Left>
vmap <Left>   <gv
nmap <Right>  >>
imap <Right>  <C-O><Right>
vmap <Right>  >gv

" toggle NERD Commenter
map <Leader>/ <Plug>NERDCommenterToggle

" toggle NERD Tree
map <Leader>n :NERDTreeToggle<CR>

" toggle Taglist
map <Leader>l :TlistToggle<CR>

" toggle spell check
map <Leader>s :set spell!<CR>

" toggle invisible character display
map <Leader>h :set list!<CR>

" run/compile file (see custom commands section)
map <Leader>r :make<CR>

"""""""""""""""""""
" Custom Commands "
"""""""""""""""""""

" configure <Leader>r shortcut (run/compile file depending on extension)
au BufNewFile,BufRead *.sh    setlocal makeprg=bash\ %
au BufNewFile,BufRead *.zsh   setlocal makeprg=zsh\ %
au BufNewFile,BufRead *.java  setlocal makeprg=javac\ %
au BufNewFile,BufRead *.rb    setlocal makeprg=ruby\ %
au BufNewFile,BufRead *.py    setlocal makeprg=python\ %
au BufNewFile,BufRead *.lua   setlocal makeprg=lua\ %
au BufNewFile,BufRead *.html  setlocal makeprg=firefox\ %

" remove trailing whitespace from all lines in the current buffer
command! Rtrim call <SID>RightTrim()
function! <SID>RightTrim()
	:% s/\s*$//g
	nohl
endfunction

" diff unsaved changes to file
if !exists(":Fdiff")
	command Fdiff vert new | set bt=nofile | r # | 0d_ | diffthis
		\ | wincmd p | diffthis
endif
