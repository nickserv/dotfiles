""""""""""
" Vundle "
""""""""""

set nocompatible " disable vi features
filetype off " required

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
Bundle 'jQuery'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-surround'
Bundle 'ervandew/supertab'
Bundle 'FuzzyFinder'
Bundle 'L9'
Bundle 'jelera/vim-javascript-syntax'
Bundle 'altercation/vim-colors-solarized'

" recommended by Vundle
"Bundle 'Lokaltog/vim-easymotion'
"Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
"Bundle 'tpope/vim-rails.git'
"Bundle 'git://git.wincent.com/command-t.git'

filetype plugin indent on " required
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

" disable folds
set nofoldenable

" enable completion
set ofu=syntaxcomplete#Complete

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

syntax enable
set background=dark
colorscheme solarized
"colorscheme Tomorrow-Night-Bright

filetype plugin indent on

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
vmap <Up>   [egv
"imap <Up>   <C-O><Up>
nmap <Down> ]e
vmap <Down> ]egv
"imap <Down> <C-O><Down>

" indent text left and right
nmap <Left>   <<
vmap <Left>   <gv
"imap <Left>   <C-O><Left>
nmap <Right>  >>
vmap <Right>  >gv
"imap <Right>  <C-O><Right>

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

"function! FdiffWrapper()
	"colorscheme Tomorrow
	"Fdiff
	"colorscheme Tomorrow-Night-Bright
"endfunction

"function! GdiffWrapper()
	"colorscheme Tomorrow
	"Gdiff
	"colorscheme Tomorrow-Night-Bright
"endfunction

"map <leader>df :FdiffWrapper
"map <leader>dg :GdiffWrapper

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
au BufNewFile,BufRead *.html  setlocal makeprg=chromium\ %

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
