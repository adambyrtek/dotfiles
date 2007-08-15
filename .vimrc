" Color scheme
colorscheme murphy

" Syntax highlighting
syntax on

" Turn off Vi compatibility
set nocompatible

" Show partial commands in status
"set showcmd

" Line numbering
"set number

" Ruler in status bar
set ruler

" No wrapping of display
"set nowrap

" Wrap lines on screen at whitespaces
set linebreak

" Distinguish case only if capital used in string
set smartcase

" No backups
set nobackup

" Smart indentation with spaces
set smarttab

" Insert tabs as spaces
set expandtab

" Number of spaces to use for indent
set shiftwidth=4

" Number of spaces to use for tab
set tabstop=4

" Highlight search in yellow
set hlsearch

" Incremental search
set incsearch

" Use UTF-8 by default
set encoding=utf-8

" Allow backspace to delete everything
set backspace=indent,eol,start

" Indent new lines
set autoindent

" Indent blocks of text
"set smartindent

" Always show tab line
"set showtabline=3

" Always show status
set laststatus=2

" Number of lines to jump during scroll
set scrolljump=5

" Number of lines before the end of the screen to scroll
set scrolloff=3

" Formatting options (see fo-table)
" ro - add comment leader on new line
" 1 - don't wrap after one-letter words
"set formatoptions+=ro1

" Buffers can be hidden
set hidden

" File type detection
filetype plugin indent on

" Show indicator on wrapped lines
set showbreak=+

" Autocommands
augroup vimrc
    au!

    " Mail mode for It's All Text
    autocmd BufNewFile,BufRead mail.google.com.* setf mail
    autocmd BufNewFile,BufRead mail.google.com.* setl tw=0

    " Mail
    autocmd FileType mail setl spell

    " LaTeX
    autocmd FileType tex compiler tex

    " Change to the directory of the file
    autocmd BufEnter * :cd %:p:h

    " Go to last known position
    autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif

augroup END

" Mappings to move through wrapped lines
imap <silent> <Down> <C-o>gj
imap <silent> <Up> <C-o>gk

nmap <silent> <Down> gj
nmap <silent> <Up> gk

" Mappings for tabs
"map <D-w> :q<CR>
"map <D-t> :tabnew<CR>
"map <D-n> :new<CR>
"map <D-S-t> :browse tabe<CR>
"map <D-S-n> :browse split<CR>
"map <D-]> :tabn<CR>
"map <D-[> :tabp<CR>

"map <D-M-Right> :tabn<CR>
"map <D-M-Left> :tabp<CR>
"imap <D-M-Right> <C-o>:tabn<CR>
"imap <D-M-Left> <C-o>:tabp<CR>
