set mouse=
set encoding=utf-8
scriptencoding utf-8
set nocompatible
" just hide buffur when abondoned
set hidden
set number
set nowrap
set nolist
set title
set showcmd           
set cmdheight=1
set laststatus=2
set tabstop=4
set expandtab
set shiftwidth=4
set backspace=indent,eol,start

au BufNewFile,BufRead \(*.js\|*.html\|*.php\) set shiftwidth=2 tabstop=2 

" keep 50 lines of command line history
set history=50
" do not creat backup files, viminfo files, and undo files.
set nobackup
set viminfo=
set noundofile
"------------------
" about search:
" do incremental searching
set incsearch
" case
set ignorecase
set smartcase
" hilight words you are searching for
set hlsearch
" go back to the top of file after searching the bottom
set wrapscan

" use mouse
set mouse=a
" in terminal
if !has("gui_running")
    set term=xterm
    set t_Co=256
    let &t_AB="\e[48;5;%dm"
    let &t_AF="\e[38;5;%dm"
endif
" show the cursor position all the time
set ruler
" move cursor automatically to where it were when you start editing new line
set autoindent
" use c style indent
set cindent

filetype plugin indent on
syntax on

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
            \ if line("'\"") >= 1 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

" edit vimrc with <Space>.
nnoremap <Space>. <C-w>n:edit $MYVIMRC<CR>
