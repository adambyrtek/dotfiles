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

" Don't distinguish case in patterns
set ignorecase

" But distinguish case if capital letter is used in pattern
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

" Show ruler with file postion
set ruler

" Show menu with available completions
set wildmenu

" Convenient completion mode
set wildmode=longest:full

" Show only manual bookmarks on the margin
let g:showmarks_include="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

" Tag list shows only tags for current file
let Tlist_Show_One_File=1

" Close tag list if it is the only window
let Tlist_Exit_OnlyWindow=1

" Tags for custom languages
" http://vim-taglist.sourceforge.net/extend.html
let tlist_actionscript_settings = 'actionscript;c:class;f:method;p:property;v:variable'
let tlist_tex_settings   = 'latex;s:sections;g:graphics;l:labels'

" Autocommands
augroup vimrc
    au!

    " Mail mode for It's All Text
    autocmd BufNewFile,BufRead mail.google.com.* setf mail
    autocmd BufNewFile,BufRead mail.google.com.* setl tw=0

    " Mail has spelling
    autocmd FileType mail setl spell

    " LaTeX compiler
    autocmd FileType tex compiler tex

    " PHP make command
    autocmd FileType php setl makeprg=php\ -l\ %
    autocmd FileType php setl errorformat=%m\ in\ %f\ on\ line\ %l

    " Change to the directory of the file
    autocmd BufRead,BufNewFile * :lcd %:p:h
    autocmd BufEnter * :lcd %:p:h

    " Go to last known position
    autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif

augroup END

" Mappings to move through wrapped lines
" http://www.vim.org/tips/tip.php?tip_id=308
inoremap <silent> <Down> <C-o>gj
inoremap <silent> <Up> <C-o>gk
nnoremap <silent> <Down> gj
nnoremap <silent> <Up> gk
vnoremap <silent> <Down> gj
vnoremap <silent> <Up> gk

" Easier omni-completion
inoremap <C-F> <C-X><C-O>

" Space can be used for paging
nnoremap <Space> <PageDown>
nnoremap <Backspace> <PageUp>

" Clever tabs
" http://www.vim.org/tips/tip.php?tip_id=102
function! CleverTab()
    if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
        return "\<Tab>"
    else
        return "\<C-N>"
endfunction
inoremap <Tab> <C-R>=CleverTab()<CR>
