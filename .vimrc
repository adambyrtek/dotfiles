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
"set hlsearch

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
"set scrolljump=5

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

" Completion similar to command line
set wildmode=list:longest

" Custom status line with filetype
set statusline=%<%f\ %h%m%r%y%=%-14.(%l,%c%V%)\ %P

" Show matching parenthesis
set showmatch

" Directories to use for swap files
set directory=~/Temp,~/tmp,/var/tmp,/tmp

" Add local directory to path (to access Subversion on Mac)
let $PATH=$PATH.":/opt/local/bin"

" Disable showing of bookmarks by default
if has("gui_running")
    let g:showmarks_enable=1
else
    let g:showmarks_enable=0
endif

" Show only manual bookmarks on the margin
let g:showmarks_include="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

" Highlight whole line when bookmark is set
let g:showmarks_hlline_lower=1
let g:showmarks_hlline_upper=1

" Tag list shows only tags for current file
let Tlist_Show_One_File=1

" Close tag list if it is the only window
let Tlist_Exit_OnlyWindow=1

" Don't show fold column in tag list
let Tlist_Enable_Fold_Column=0

" Show tag list window on right
let Tlist_Use_Right_Window=1

" Focus tag list window when opened
let Tlist_GainFocus_On_ToggleOpen=1

" Tags for custom languages
" http://vim-taglist.sourceforge.net/extend.html
let tlist_actionscript_settings='actionscript;c:class;f:method;p:property;v:variable'
let tlist_tex_settings='latex;s:sections;g:graphics;l:labels'

" NERD Commenter won't output "Unknown filetype" messages
let NERDShutUp=1

" Qbuf hotkey
let g:qb_hotkey = "<Leader>q"

" Autocommands
augroup vimrc
    au!

    " Mail mode for It's All Text
    autocmd BufNewFile,BufRead mail.google.com.* setf mail

    " Mail has spelling and wrapping
    autocmd FileType mail setl spell
    autocmd FileType mail setl wrap
    autocmd FileType mail setl tw=0

    " LaTeX compiler
    autocmd FileType tex compiler tex

    " PHP syntaxt error checking
    autocmd FileType php setl makeprg=php5\ -l\ %
    autocmd FileType php setl errorformat=%m\ in\ %f\ on\ line\ %l

    " Python syntax error checking
    " http://blog.sontek.net/2008/05/11/python-with-a-modular-ide-vim/
    autocmd FileType python setl makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"
    autocmd FileType python setl efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m

    " Ruby indentation
    autocmd FileType ruby setl shiftwidth=2
    autocmd FileType eruby setl shiftwidth=2

    " Correct type for ERB files
    autocmd BufNewFile,BufRead *.erb setl ft=eruby

    " YAML indentation
    autocmd FileType yaml setl shiftwidth=2

    " Change to the directory of the file
    "autocmd BufRead,BufNewFile * :lcd %:p:h
    "autocmd BufEnter * :lcd %:p:h

    " Go to last known position
    autocmd BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") |
                \     exe "normal g`\"" |
                \ endif

    " ShowMarks highlight colors
    autocmd VimEnter *
                \ if has('gui') |
                \     highlight ShowMarksHLl guibg=#660000 |
                \     highlight ShowMarksHLu guibg=#660000 |
                \     highlight ShowMarksHLo guibg=#660000 |
                \     highlight ShowMarksHLm guibg=#660000 |
                \     highlight SignColumn   guibg=#111111 |
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
vnoremap ,) di()<Esc>P2l
vnoremap ,} di{}<Esc>P2l
vnoremap ,] di[]<Esc>P2l
vnoremap ,> di<><Esc>P2l
vnoremap ," di""<Esc>P2l
vnoremap ,' di''<Esc>P2l

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

" Mapping for tab completion
inoremap <Tab> <C-R>=InsertTabWrapper("forward")<CR>
inoremap <S-Tab> <C-R>=InsertTabWrapper("backward")<CR>

" Leader shortcuts to frequent actions
nnoremap <Leader>a :b#<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>W :wall<CR>
nnoremap <Leader>d :bd<CR>
nnoremap <Leader>D :BD<CR>
" nnoremap <Leader>m :w<CR>:make<CR>:cw<CR>
nnoremap <Leader>t :TlistToggle<CR>
nnoremap <Leader>T :!ctags -R .<CR>
nnoremap <Leader>n :NERDTreeToggle<CR>
nnoremap <Leader>e :BufExplorer<CR>
