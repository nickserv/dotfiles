""""""""
" Plug "
""""""""

call plug#begin('~/.vim/plugged')

" Features
Plug 'tpope/vim-sensible'
"Plug 'taglist.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'Townk/vim-autoclose'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-rake'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-unimpaired'
"Plug 'tpope/vim-surround'
"Plug 'ervandew/supertab'
Plug 'scrooloose/syntastic'
"Plug 'Lokaltog/vim-easymotion'
"Plug 'mattn/invader-vim'
"Plug 'duellj/DirDiff.vim'
"Plug 'Shougo/neocomplcache'
"Plug 'kien/ctrlp.vim'
Plug 'kien/rainbow_parentheses.vim'
Plug 'mattn/emmet-vim'

" Version control
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Syntax
Plug 'kchmck/vim-coffee-script'
Plug 'jQuery'
Plug 'tpope/vim-git'
Plug 'tpope/vim-liquid'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-haml'
Plug 'hail2u/vim-css3-syntax'
Plug 'jelera/vim-javascript-syntax'
Plug 'vim-ruby/vim-ruby'
Plug 'digitaltoad/vim-jade'
Plug 'jnwhiteh/vim-golang'
Plug 'othree/html5.vim'
Plug 'moll/vim-node'

call plug#end()

""""""
" UI "
""""""

" show the filename in the window titlebar
set title

" set encoding
set encoding=utf-8

" directories for swp files
set backupdir=~/.vim/backup
set directory=~/.vim/backupf

" display incomplete commands at the bottom
set showcmd

" mouse support
set mouse=a

" line numbers
set number

" highlight cursor line
set cursorline

" wrapping stuff
set textwidth=80
set colorcolumn=80

" ignore whitespace in diff mode
set diffopt+=iwhite

" force setting wrap when vim diff opens
autocmd FilterWritePre * if &diff | setlocal wrap< | endif

" remember last cursor position
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" show '>   ' at the beginning of lines that are automatically wrapped
set showbreak=>\ \ \ 

" disable folds
set nofoldenable

" enable completion
set ofu=syntaxcomplete#Complete

" make laggy connections work faster
set ttyfast

" let vim open up to 100 tabs at once
set tabpagemax=100

" case-insensitive filename completion
set wildignorecase

" briefly jump to matching braces when you insert them
set showmatch

" GitGutter
let g:gitgutter_diff_args = '-w'
" make the signs column look the same as the line number column
" disable eager loading
let g:gitgutter_eager = 0

" rainbow parentheses
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

"""""""""""""
" Searching "
"""""""""""""

set hlsearch "when there is a previous search pattern, highlight all its matches
set ignorecase "ignore case in search patterns
set smartcase "override the 'ignorecase' option if the search pattern contains uppercase characters
set gdefault "imply global for new searches

"""""""""""""
" Indenting "
"""""""""""""

" Number of spaces that a <Tab> in the file counts for
set tabstop=2
" Number of spaces to use for each step of (auto)indent
set shiftwidth=2
" Use spaces for indents
set expandtab
" When auto-indenting, use the indenting format of the previous line
set copyindent
" Automatically inserts one extra level of indentation in some cases, and works
" for C-like files
set smartindent

"""""""""
" Theme "
"""""""""

" Reset the background color of the sign column so it doesn't look ugly
highlight clear SignColumn

set guifont=Source\ Code\ Pro:h14

""""""""
" GVim "
""""""""

if has('gui_running')
  set gcr=n:blinkon0 "disable cursor blinking

  set guioptions-=m "remove menu bar
  set guioptions-=T "remove toolbar
  set guioptions-=L "remove left scroll bar
  set guioptions-=r "remove right scroll bar
endif

""""""""""""
" Mappings "
""""""""""""

" change leader from \ to ,
let mapleader=","

" typing ;; quickly exits insert mode
imap ;; <Esc>

" git
"map <Leader>gg :GitGutterToggle<CR>
"map <Leader>gh :GitGutterLineHighlightsToggle<CR>

" bubble individual lines
nmap <Up>   [e
nmap <Down> ]e
" bubble multiple lines
vmap <Up>   [egv
vmap <Down> ]egv

" indent individual lines
nmap <Left>   <<
nmap <Right>  >>
" indent multiple lines
vmap <Left>   <gv
vmap <Right>  >gv

" delete individual lines with ctrl-k
map  <C-K>    dd
imap <C-K>    <C-O>dd

" toggle NERD Commenter
" (for some reason, these seem to also bind to C-/ in some terminals)
map  <C-_>    <Plug>NERDCommenterToggle
imap <C-_>    <C-O><Plug>NERDCommenterToggle
nmap <C-_>    <Plug>NERDCommenterToggle:normal j<CR>

" toggle NERD Tree
map <Leader>n :NERDTreeToggle<CR>

" toggle Taglist
map <Leader>l :TlistToggle<CR>

" toggle spell check
map <Leader>s :set spell!<CR>

" diffs!
map <Leader>dd :diffupdate<CR>
map <Leader>df :Fdiff<CR>
map <Leader>dg :Gdiff<CR>
map <Leader>dh :Hgvdiff<CR>

" more spell check shortcuts
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=

" toggle invisible character display
map <Leader>h :set list!<CR>

" run/compile file (see custom commands section)
map <Leader>r :make<CR>

" Format the current paragraph according to
" the current 'textwidth' with ctrl-j:
nmap <C-J>      gqap
vmap <C-J>      gq
imap <C-J>      <C-O>gqap

" tabs
map <Leader>tp :tabp<CR>
map <Leader>tn :tabnext<CR>

" opens an edit command with the path of the currently edited file filled in
" normal mode: <Leader>e
map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" opens a tab edit command with the path of the currently edited file filled in
" normal mode: <Leader>t
map <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>

" inserts the path of the currently edited file into a command
" command mode: Ctrl+P
cmap <C-P> <C-R>=expand("%:p:h") . "/" <CR>

"""""""""""""""""""""
" Language-Specific "
"""""""""""""""""""""

" configure <Leader>r shortcut (run/compile file depending on extension)
au BufNewFile,BufRead *.sh    setlocal makeprg=bash\ %
au BufNewFile,BufRead *.zsh   setlocal makeprg=zsh\ %
au BufNewFile,BufRead *.java  setlocal makeprg=javac\ %
au BufNewFile,BufRead *.rb    setlocal makeprg=ruby\ %
au BufNewFile,BufRead *.js    setlocal makeprg=node\ %
au BufNewFile,BufRead *.py    setlocal makeprg=python\ %
au BufNewFile,BufRead *.lisp  setlocal makeprg=clisp\ %
au BufNewFile,BufRead *.scm   setlocal makeprg=guile\ %
au BufNewFile,BufRead *.lua   setlocal makeprg=lua\ %
au BufNewFile,BufRead *.html  setlocal makeprg=google-chrome\ %
au BufNewFile,BufRead *.hs    setlocal makeprg=runhaskell\ %
au BufNewFile,BufRead *.go    setlocal makeprg=go\ run\ %

" Thorfile, Rakefile, Vagrantfile and Gemfile are Ruby
au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,config.ru} setlocal ft=ruby
au BufRead,BufNewFile *.html.erb setlocal ft=eruby

" ASDF files are Common Lisp
au BufRead,BufNewFile *.asd setlocal ft=lisp

" Add json syntax highlighting
au BufNewFile,BufRead *.json setlocal syntax=javascript

" Syntastic checkers
let g:syntastic_javascript_checkers=['jslint']
"let g:syntastic_javascript_jslint_args="--my --args --here"
let g:syntastic_html_tidy_ignore_errors=[" proprietary attribute \"ng-"]

" comfortable prose editing
au FileType gitcommit setlocal spell
au FileType hgcommit  setlocal spell
au FileType txt       setlocal spell
au FileType md        setlocal spell
au FileType html      setlocal spell
au FileType yaml      setlocal spell
au FileType xml       setlocal spell

"""""""""""""""""""
" Custom Commands "
"""""""""""""""""""

" remove trailing whitespace from all lines in the current buffer
command! Rtrim call <SID>RightTrim()
function! <SID>RightTrim()
  :% s/\s*$//g
  nohl
endfunction

" diff unsaved changes to file
command! Fdiff vert new | set bt=nofile | r # | 0d_ | diffthis
  \ | wincmd p | diffthis

" indents: convert spaces to tabs
command! -range=% -nargs=0 IndentTabs execute '<line1>,<line2>s#^\( \{'.&ts.'\}\)\+#\=repeat("\t", len(submatch(0))/' . &ts . ')'

" indents: convert tabs to spaces
command! -range=% -nargs=0 IndentSpaces execute '<line1>,<line2>s#^\t\+#\=repeat(" ", len(submatch(0))*' . &ts . ')'

" highlight duplicated code (see http://stackoverflow.com/a/1270689/406249)
command! Dupes syn clear Repeat | g/^\(.*\)\n\ze\%(.*\n\)*\1$/exe 'syn match Repeat "^' . escape(getline('.'), '".\^$*[]') . '$"' | nohlsearch

set cursorcolumn
