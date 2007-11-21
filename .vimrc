" Color scheme
colorscheme pablo

" Syntax highlighting
syntax on

" Turn off Vi compatibility
set nocompatible

" Show partial commands in status
set showcmd

" Line numbering
"set number

" No wrapping of display
set nowrap

" Wrap lines on screen at whitespaces
set linebreak

" Don't distinguish case in patterns
set ignorecase

" But distinguish case if capital letter is used in pattern
set smartcase

" No backups
set nobackup

" Indentation can be deleted with Backspace
set smarttab

" Indent and tab with spaces
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

" Indent new lines as previous line
set autoindent

" Indent blocks of text
"set smartindent

" Always show tab list
"set showtabline=3

" Always show status
set laststatus=2

" Number of lines to jump during scroll
set scrolljump=5

" Number of lines before the end of the screen to scroll
set scrolloff=3

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

" Custom status line with filetype
set statusline=%<%f\ %h%m%r%y%=%-14.(%l,%c%V%)\ %P

" Add local directory to path (to access Subversion on Mac)
let $PATH=$PATH.":/usr/local/bin"

" Show only manual bookmarks on the margin
let g:showmarks_include="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

" Tag list shows only tags for current file
"let Tlist_Show_One_File=1

" Close tag list if it is the only window
let Tlist_Exit_OnlyWindow=1

" Tags for custom languages
" http://vim-taglist.sourceforge.net/extend.html
let tlist_actionscript_settings = 'actionscript;c:class;f:method;p:property;v:variable'
let tlist_tex_settings = 'latex;s:sections;g:graphics;l:labels'

" Autocommands
augroup vimrc
    au!

    " Mail mode for It's All Text
    autocmd BufNewFile,BufRead mail.google.com.* setf mail
    autocmd BufNewFile,BufRead mail.google.com.* setl tw=0

    " Mail has spelling and wrapping
    autocmd FileType mail setl spell
    autocmd FileType mail setl wrap

    " LaTeX compiler
    autocmd FileType tex compiler tex

    " PHP make command
    autocmd FileType php setl makeprg=php5\ -l\ %
    autocmd FileType php setl errorformat=%m\ in\ %f\ on\ line\ %l

    " Ruby indentation
    autocmd FileType ruby setl shiftwidth=2
    autocmd FileType eruby setl shiftwidth=2

    " YAML indentation
    autocmd FileType yaml setl shiftwidth=2

    " Change to the directory of the file
    autocmd BufRead,BufNewFile * :lcd %:p:h
    autocmd BufEnter * :lcd %:p:h

    " Omni completion enabled
    " http://amix.dk/blog/viewEntry/19021
    autocmd FileType python setl omnifunc=pythoncomplete#Complete
    autocmd FileType javascript setl omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType html setl omnifunc=htmlcomplete#CompleteTags
    autocmd FileType css setl omnifunc=csscomplete#CompleteCSS
    autocmd FileType xml setl omnifunc=xmlcomplete#CompleteTags
    autocmd FileType php setl omnifunc=phpcomplete#CompletePHP
    autocmd FileType c setl omnifunc=ccomplete#Complete

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

" Surround visual selection
vmap ,) di()<Esc>P2l
vmap ,} di{}<Esc>P2l
vmap ,] di[]<Esc>P2l
vmap ,/ di//<Esc>P2l
vmap ," di""<Esc>P2l
vmap ,' di''<Esc>P2l
vmap ,< di<<Esc>pa></<Esc>pa><Esc>F/hi

" Clever tabs
function! InsertTabWrapper(direction)
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<Tab>"
    elseif "backward" == a:direction
        return "\<C-P>"
    else
        return "\<C-N>"
    endif
endfunction

inoremap <Tab> <C-R>=InsertTabWrapper("forward")<CR>
inoremap <S-Tab> <C-R>=InsertTabWrapper("backward")<CR>
