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

    " Select color scheme
    colorscheme gruvbox

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
set ttimeoutlen=50

" Shorter delay before swap write and cursor hold event
set updatetime=1000

" Color column to mark long lines
" set colorcolumn=100

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

" Variables {{{1

" Always start search from cwd
let g:ctrlp_working_path_mode = 'rw'

" Include current file in search results
let g:ctrlp_match_current_file = 1

" Use Ag for building CtrlP index (if available)
if executable('ag')
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
    let g:ctrlp_use_caching = 0
endif

" Prevent netrw from loading
let loaded_netrwPlugin = 1

" Use relative paths for consistency
let g:dirvish_relative_paths = 1

" Advanced word motions
let g:wordmotion_prefix = ','

" Linter settings
let g:syntastic_python_checkers = ['python', 'flake8']
let g:syntastic_ruby_checkers = ['mri', 'rubocop']

" Auto-populate location list with linter results
let g:syntastic_always_populate_loc_list = 1

" Refresh signs in real-time without writing files
let g:signify_realtime = 1
let g:signify_cursorhold_normal = 0
let g:signify_cursorhold_insert = 0

" NeoVim will use system Python (not one from virtualenv)
if has('nvim')
    let g:python_host_prog = '/usr/bin/python'
    let g:python3_host_prog = '/usr/bin/python3'
end

" Lightline configuration
let g:lightline = {
            \     'colorscheme': 'gruvbox',
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
end

" Autocommands {{{1

augroup vimrc
    autocmd!

    " Python
    if executable('isort')
        autocmd Filetype python setl formatprg=isort\ -
    end
    if executable('py.test')
        autocmd Filetype python compiler pytest
    end

    " Ruby
    autocmd Filetype ruby setl shiftwidth=2 softtabstop=2

    " JavaScript
    autocmd Filetype javascript setl shiftwidth=2 softtabstop=2

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

" Select last put
nnoremap <expr> <Leader>v '`[' . getregtype()[0] . '`]'

" Syntastic toggle
nnoremap <Leader>s :SyntasticToggleMode<CR>

" Custom command and mapping for Ag (if available)
if executable('ag')
    command! -nargs=+ -complete=file -bar Ag silent grep! <args> | botright cwindow | redraw!
    nnoremap <Leader>a :Ag<Space>
    nnoremap <Leader>A :Ag<Space>-w<Space>'<C-r><C-w>'
endif

" Buffer search
nnoremap <C-b> :CtrlPBuffer<CR>

" Cycle between windows with Backspace
nnoremap <BS> <C-w>w

" Emacs bindings for the command line (see :h emacs-keys)
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
cnoremap <C-d> <Del>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>
cnoremap <C-g> <C-c>

" Signify text object
omap ic <plug>(signify-motion-inner-pending)
xmap ic <plug>(signify-motion-inner-visual)
omap ac <plug>(signify-motion-outer-pending)
xmap ac <plug>(signify-motion-outer-visual)

" Close instead of quit
cabbrev q <C-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'close' : 'q')<CR>

" Abbreviate current directory
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Easy exit from terminal mode
if has('nvim')
    tnoremap <Esc> <C-\><C-n>
endif

" Delete the buffer and preserve the window
nnoremap <Leader>d :Sayonara!<CR>
