" vim: foldmethod=marker

" Packages {{{1

" Use minipac for managing packages
packadd minpac
call minpac#init({'verbose': 3})

call minpac#add('5long/pytest-vim-compiler')
call minpac#add('Vimjas/vim-python-pep8-indent')
call minpac#add('airblade/vim-gitgutter')
call minpac#add('chaoren/vim-wordmotion')
call minpac#add('ctrlpvim/ctrlp.vim')
call minpac#add('davidhalter/jedi-vim')
call minpac#add('itchyny/lightline.vim')
call minpac#add('justinmk/vim-dirvish')
call minpac#add('kana/vim-textobj-entire')
call minpac#add('kana/vim-textobj-indent')
call minpac#add('kana/vim-textobj-user')
call minpac#add('mhinz/vim-sayonara')
call minpac#add('morhetz/gruvbox')
call minpac#add('nanotech/jellybeans.vim')
call minpac#add('radenling/vim-dispatch-neovim')
call minpac#add('tpope/vim-commentary')
call minpac#add('tpope/vim-dispatch')
call minpac#add('tpope/vim-fugitive')
call minpac#add('tpope/vim-repeat')
call minpac#add('tpope/vim-surround')
call minpac#add('tpope/vim-unimpaired')
call minpac#add('w0rp/ale')

call minpac#add('edkolev/tmuxline.vim', {'type': 'opt'})

" Appearance {{{1

" Default color schemes
if has('gui_running') || &t_Co >= 256
    " Enable true color if supported
    if has('termguicolors')
        set termguicolors
    endif

    " Select color scheme
    let g:gruvbox_italic = 1
    colorscheme gruvbox
    " colorscheme jellybeans

    " Background has to be set after color scheme
    set background=dark
else
    " Looks decent with default terminal colors
    colorscheme elflord
endif

" Settings {{{1

" Syntax highlighting
syntax on

" File type detection
filetype plugin indent on

" Turn off Vi compatibility
set nocompatible

" Internal encoding
set encoding=utf-8

" Show partial commands in status
set showcmd

" No soft word wrapping
set nowrap

" Soft wrap lines at whitespace (when enabled)
set linebreak

" Display incomplete wrapped lines at the end of the screen
set display+=lastline

" Case insensitive search and completion
set ignorecase

" ...except when a capital letter is used
set smartcase

" Use spaces instead of tabs
set expandtab

" Number of spaces to use for indent
set shiftwidth=4

" Number of spaces to use for Tab in insert mode
set softtabstop=4

" Tab in insert mode indents with spaces
set smarttab

" Indent new lines as previous line
set autoindent

" Incremental search
set incsearch

" Highlight search results
set hlsearch

" Allow backspace to delete everything
set backspace=indent,eol,start

" Always show status
set laststatus=2

" Number of lines before the end of the screen to scroll
set scrolloff=6

" Buffers can be hidden
set hidden

" Show indicator on wrapped lines
set showbreak=+

" Show ruler with file position
set ruler

" Completion similar to command line
set wildmode=longest:full,full

" Show completion menu
set wildmenu

" Saner insert mode completion
set completeopt=longest,menuone

" Show matching parenthesis
set showmatch

" No swap files
set noswapfile

" Always display special characters
set list

" Special characters to show
set listchars=tab:>·,trail:·,extends:>,precedes:<

" Default global replace
set gdefault

" Longer history
set history=1000

" Use one space instead of two when joining sentences
set nojoinspaces

" Remove comment character when joining comment lines
set formatoptions+=j

" Reload file when changed (and not edited in Vim)
set autoread

" Shorter timeout after Esc in classic Vim
if !has('nvim')
    set ttimeout
    set ttimeoutlen=50
endif

" Shorter cursor hold timeout (e.g. for gitgutter)
set updatetime=250

" Recursive find
set path+=**
set path-=/usr/include

" Avoid parsing all include files
set complete-=i

" Magic to make true color work in tmux
set t_8f=[38;2;%lu;%lu;%lum
set t_8b=[48;2;%lu;%lu;%lum

" Use Ag instead of grep (if available)
if executable('ag')
    set grepprg=ag\ --vimgrep
    set grepformat^=%f:%l:%c:%m
endif

" More intuitive splits
set splitright splitbelow

" Variables {{{1

" Change leader to work better with different layouts
let mapleader = ','

" Always start search from cwd
let g:ctrlp_working_path_mode = 'rw'

" Include current file in search results
let g:ctrlp_match_current_file = 1

" Use Ag for building CtrlP index (if available)
if executable('ag')
    let g:ctrlp_user_command = 'ag %s --nocolor -g ""'
    let g:ctrlp_use_caching = 0
endif

" Mapping to delete buffers
let g:ctrlp_prompt_mappings = { 'PrtDeleteEnt()': ['<c-k>'] }

" Prevent netrw from loading
let loaded_netrwPlugin = 1

" Use relative paths for consistency
let g:dirvish_relative_paths = 1

" Advanced word motions
let g:wordmotion_prefix = ','

" Linter settings
let g:ale_linters = {
            \     'python': ['flake8', 'mypy'],
            \ }

" NeoVim will use system Python (not one from virtualenv)
if has('nvim')
    let g:python_host_prog = '/usr/bin/python'
    let g:python3_host_prog = '/usr/bin/python3'
endif

" Lightline configuration
let g:lightline = {
            \     'colorscheme': g:colors_name,
            \     'active': {
            \         'left': [ [ 'mode', 'paste' ], [ 'relativepath', 'modified' ], [ 'fugitive' ] ],
            \         'right': [ [ 'lineinfo' ], [ 'percent' ], [ 'filetype', 'spell', 'readonly' ] ]
            \     },
            \     'inactive': {
            \         'left': [ [ 'relativepath', 'modified' ] ],
            \         'right': [ [ 'lineinfo' ], [ 'percent' ] ]
            \     },
            \     'component': {
            \         'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
            \     },
            \     'component_visible_condition': {
            \         'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
            \     }
            \ }

" Standard statusline colors consistent with lightline
if g:colors_name == "jellybeans"
    highlight! link StatusLine LightlineMiddle_normal
endif

" Jedi configuration
let g:jedi#auto_vim_configuration = 0

" Autocommands {{{1

augroup vimrc
    autocmd!

    " Python formatting
    if executable('isort')
        autocmd Filetype python setl formatprg=isort\ -
    endif

    " Python test dispatch
    if executable('py.test')
        autocmd Filetype python let b:dispatch = 'py.test -q %'
    endif

    " Ruby
    autocmd Filetype ruby setl sw=2 sts=2

    " JavaScript
    autocmd Filetype javascript setl sw=2 sts=2

    " Shortcut to close quickfix and help
    autocmd Filetype qf,help nnoremap <buffer> q :close<CR>

    " Spell check Git commit message
    autocmd Filetype gitcommit setl spell spl=en

    " Highlight the cursor line in the current window only
    autocmd WinEnter,BufWinEnter * setl cursorline
    autocmd WinLeave,BufWinLeave * setl nocursorline

    " Regularly check for external modifications
    autocmd BufEnter,WinEnter,FocusGained,CursorHold * silent! checktime

    " Restore last cursor position
    autocmd BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") |
                \     exe "normal g`\"" |
                \ endif
augroup END

" Commands and mappings {{{1

" Formatting instead of ex mode
nnoremap Q gq
vnoremap Q gq

" Marks include column
nnoremap ' `
vnoremap ' `

" Clear highlights
nnoremap <Leader><Leader> :nohlsearch<CR>

" Open/close quickfix windows
nnoremap <Leader>q :botright copen<CR>
nnoremap <Leader>Q :cclose<CR>
nnoremap <Leader>l :lopen<CR>
nnoremap <Leader>L :lclose<CR>

" Paste last yank
nnoremap <Leader>p "0p
nnoremap <Leader>P "0P
vnoremap <Leader>p "0p
vnoremap <Leader>P "0P

" Very magic search
nnoremap <Leader>/ /\v

" Custom command and mapping for Ag (if available)
if executable('ag')
    command! -nargs=+ -complete=file -bar Ag silent grep! <args> | botright cwindow | redraw!
    nnoremap <Leader>a :Ag<Space>
    nnoremap <Leader>A :Ag<Space>-w<Space>'<C-r><C-w>'
endif

" Buffer search
nnoremap <C-b> :CtrlPBuffer<CR>

" Cycle between windows
nnoremap <C-j> <C-w>w
nnoremap <C-k> <C-w>W

" Emacs bindings for the command line (see :h emacs-keys)
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-d> <Del>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>

" Make C-c trigger autocmds
inoremap <C-c> <Esc>

" Abbreviate current directory
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Easy exit from terminal mode
if has('nvim')
    tnoremap <Esc> <C-\><C-n>
endif

" Delete the buffer and preserve the window
nnoremap <Leader>bd :Sayonara!<CR>

" Dispatch mappings
nnoremap <Leader>x :Dispatch<CR>
nnoremap <Leader>X :Dispatch<Space>

" Fugitive mappings
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>gC :Gcommit -v<CR>
