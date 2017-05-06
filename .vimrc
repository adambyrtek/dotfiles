" vim: foldmethod=marker

" Pathogen {{{1

" Initialize Pathogen, has to be at the top
" (make sure to run :Helptags after installing new plugins)
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()

" Appearance {{{1

" Default color schemes
if has('gui_running') || &t_Co >= 256
    " Enable true color if supported
    if has('termguicolors')
        set termguicolors
    endif

    " Actively maintaned dark scheme
    colorscheme jellybeans
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

" Tab in insert mode indents with spaces
set smarttab

" Indent new lines as previous line
set autoindent

" Number of spaces to use for indent
set shiftwidth=4

" Number of spaces to use for Tab when editing
set softtabstop=4

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
set wildmode=list:longest

" Saner insert mode completion
set completeopt=longest,menuone,preview

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

" Shorter timeout after Esc
set ttimeoutlen=100

" Color column to mark long lines
set colorcolumn=100

" Clear screen using background color from the scheme
set t_ut=

" Variables {{{1

" Leader key bindings
let mapleader = '\'
let maplocalleader = ','

" Higlight search term
let g:ackhighlight = 1

" Use Ag instead of Ack if available
if executable('ag')
    let g:ackprg = 'ag --vimgrep'
endif

" Always start search from cwd
let g:ctrlp_working_path_mode = 'rw'

" Include current file in search results
let g:ctrlp_match_current_file = 1

" Use Ag for building CtrlP index if available
if executable('ag')
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
    let g:ctrlp_use_caching = 0
endif

" Advanced word motions
let g:wordmotion_prefix = ','

" Lightline configuration
let g:lightline = {
            \     'colorscheme': 'jellybeans',
            \     'active': {
            \         'left': [ [ 'mode', 'paste' ], [ 'readonly', 'relativepath', 'modified' ], [ 'fugitive' ] ],
            \         'right': [ [ 'lineinfo' ], [ 'percent' ], [ 'fileformat', 'fileencoding', 'filetype', 'spell' ] ]
            \     },
            \     'component': {
            \         'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
            \     },
            \     'component_visible_condition': {
            \         'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
            \     }
            \ }

" Standard statusline colors consistent with lightline
highlight! link StatusLine LightlineMiddle_normal

" Autocommands {{{1

augroup vimrc
    autocmd!

    " Highlight the cursor line in the current window only
    autocmd VimEnter,WinEnter * set cursorline
    autocmd WinLeave * set nocursorline

    " Regularly check for external modifications
    autocmd BufEnter,WinEnter,FocusGained,CursorHold * silent! checktime
    "
    " Regularly check for repository changes
    autocmd BufEnter,WinEnter,FocusGained,CursorHold * SignifyRefresh

    " Ruby indentation
    autocmd Filetype ruby setl shiftwidth=2

    " Restore last cursor position
    autocmd BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") |
                \     exe "normal g`\"" |
                \ endif

augroup END

" Commands and mappings {{{1

" Better use of Q instead of ex mode
nnoremap Q gqap
vnoremap Q gq

" Clear highlights
nnoremap <Leader><Leader> :nohlsearch<CR>

" Edit file based on current file or directory
nnoremap <Leader>e :e <C-R>=expand('%:h')<CR>/
nnoremap <Leader>E :e <C-R>=expand('%')<CR>

" Open/close quickfix
nnoremap <Leader>q :botright copen<CR>
nnoremap <Leader>Q :cclose<CR>

" Paste last yank
nnoremap <Leader>p "0p
nnoremap <Leader>P "0P

" Toggles
nnoremap <Leader>gp :set paste!<CR>:set paste?<CR>
nnoremap <Leader>gh :set hlsearch!<CR>:set hlsearch?<CR>
nnoremap <Leader>gn :setl number!<CR>:setl number?<CR>
nnoremap <Leader>gl :setl list!<CR>:setl list?<CR>
nnoremap <Leader>gs :setl spell!<CR>:setl spell?<CR>
nnoremap <Leader>gw :setl wrap!<CR>:setl wrap?<CR>

" Buffer search
nnoremap <C-b> :CtrlPBuffer<CR>

" Emacs bindings for the command line (:help emacs-keys)
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-d> <Del>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>

" Bracket quickfix
nnoremap [Q :cfirst<CR>
nnoremap ]Q :clast<CR>
nnoremap [q :cprevious<CR>
nnoremap ]q :cnext<CR>

" Bracket location
nnoremap [L :lfirst<CR>
nnoremap ]L :llast<CR>
nnoremap [l :lprevious<CR>
nnoremap ]l :lnext<CR>

" Bracket tags
nnoremap [T :tfirst<CR>
nnoremap ]T :tlast<CR>
nnoremap [t :tprevious<CR>
nnoremap ]t :tnext<CR>

" Bracket buffers
nnoremap [B :bfirst<CR>
nnoremap ]B :blast<CR>
nnoremap [b :bprevious<CR>
nnoremap ]b :bnext<CR>

" Signify text object
omap ic <plug>(signify-motion-inner-pending)
xmap ic <plug>(signify-motion-inner-visual)
omap ac <plug>(signify-motion-outer-pending)
xmap ac <plug>(signify-motion-outer-visual)
