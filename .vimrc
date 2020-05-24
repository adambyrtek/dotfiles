" vim: foldmethod=marker

" Packages {{{1

" Use minipac for managing packages
packadd minpac

if exists('*minpac#init')
    call minpac#init()

    " Async completions
    call minpac#add('Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'})
    " call minpac#add('Shougo/context_filetype.vim')
    call minpac#add('Shougo/echodoc.vim')
    call minpac#add('Shougo/neco-vim')
    call minpac#add('deoplete-plugins/deoplete-jedi')
    " call minpac#add('deoplete-plugins/deoplete-go', {'do': 'make'})

    call minpac#add('Vimjas/vim-python-pep8-indent')
    call minpac#add('airblade/vim-gitgutter')
    call minpac#add('cespare/vim-toml')
    call minpac#add('chaoren/vim-wordmotion')
    call minpac#add('ctrlpvim/ctrlp.vim')
    call minpac#add('davidhalter/jedi-vim')
    call minpac#add('hashivim/vim-terraform')
    call minpac#add('itchyny/lightline.vim')
    call minpac#add('justinmk/vim-dirvish')
    call minpac#add('kana/vim-textobj-entire')
    call minpac#add('kana/vim-textobj-indent')
    call minpac#add('kana/vim-textobj-user')
    call minpac#add('mhinz/vim-sayonara')
    call minpac#add('morhetz/gruvbox')
    call minpac#add('radenling/vim-dispatch-neovim')
    call minpac#add('tartansandal/vim-compiler-pytest')
    call minpac#add('tpope/vim-commentary')
    call minpac#add('tpope/vim-dispatch')
    call minpac#add('tpope/vim-eunuch')
    call minpac#add('tpope/vim-fugitive')
    call minpac#add('tpope/vim-repeat')
    call minpac#add('tpope/vim-rhubarb')
    call minpac#add('tpope/vim-surround')
    call minpac#add('tpope/vim-unimpaired')
    call minpac#add('w0rp/ale')

    call minpac#add('edkolev/tmuxline.vim', {'type': 'opt'})
endif

" Appearance {{{1

" Enable true color support
set termguicolors

" Gruvbox can support 256 colors and true color
let g:gruvbox_italic = 1
colorscheme gruvbox

" Has to be set after loading Gruvbox
set background=dark

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

" Do not show current mode in the command line
set noshowmode

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

" Incremental substitution
if has('nvim')
    set inccommand=nosplit
endif

" Highlight search results
set hlsearch

" Allow backspace to delete everything
set backspace=indent,eol,start

" Always show status
set laststatus=2

" Number of lines before the end of the screen to scroll
set scrolloff=8

" Buffers can be hidden
set hidden

" Show indicator on wrapped lines
set showbreak=+

" Show ruler with file position
set ruler

" Command mode completion
set wildmode=longest:full,full

" Show completion menu
set wildmenu

" Better insert mode completion ('longest' is hard to undo)
set completeopt=menuone,noselect

" Do not show completion messages on the command line
set shortmess+=c

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

" Shorter cursor hold timeout (e.g. for Git gutter)
set updatetime=100

" Recursive find
set path+=**
set path-=/usr/include

" Avoid parsing all include files
set complete-=i

" Magic to make true color work in tmux
if !has('nvim')
    set t_8f=[38;2;%lu;%lu;%lum
    set t_8b=[48;2;%lu;%lu;%lum
endif

" Use Ag instead of grep (if available)
if executable('ag')
    set grepprg=ag\ --vimgrep
    set grepformat^=%f:%l:%c:%m
endif

" More intuitive vertical split
set splitright

" Disable modelines for security reasons
set nomodeline

" Always show the gutter
set signcolumn=yes

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
let g:ctrlp_prompt_mappings = {'PrtDeleteEnt()': ['<c-k>']}

" Prevent netrw from loading
let loaded_netrwPlugin = 1

" Use relative paths for consistency
let g:dirvish_relative_paths = 1

" Advanced word motions
let g:wordmotion_prefix = ','

" ALE linters
let g:ale_linters = {}

" ALE fixers
let g:ale_fixers = {
            \   '*': ['remove_trailing_lines', 'trim_whitespace'],
            \   'python': ['isort', 'black'],
            \   'javascript': ['eslint'],
            \   'json': ['jq'],
            \   'terraform': ['terraform'],
            \}

" Linter name in the error message
let g:ale_echo_msg_format = '[%linter%] %code: %%s'

" Neovim should use system Python (not one from virtualenv)
if has('nvim')
    let g:python_host_prog = '/usr/bin/python2'
    let g:python3_host_prog = '/usr/bin/python3'
endif

" Git status
function! LightlineGitGutter()
    if &buftype ==# '' && exists('*GitGutterGetHunkSummary')
        let [a, m, r] = GitGutterGetHunkSummary()
        if a + m + r > 0
            return printf('+%d ~%d -%d', a, m, r)
        end
    end
    return ''
endfunction

" Shorten file name
function! LightlineShortName()
    if &filetype ==# 'fugitive' || expand('%') =~? '^fugitive:'
        return FugitiveStatusline()
    elseif &filetype ==# 'qf'
        return exists('w:quickfix_title') ? w:quickfix_title : 'quickfix'
    else
        let shortname = expand('%:~:.')
        return shortname != '' ? shortname : expand('%:~')
    end
endfunction

" Lightline configuration
let g:lightline = {
            \   'colorscheme': 'gruvbox',
            \   'active': {
            \       'left': [['mode', 'paste'], ['shortname', 'modified'], ['fugitive', 'gitgutter' ]],
            \       'right': [['lineinfo'], ['percent'], ['filetype', 'spell', 'readonly']],
            \   },
            \   'inactive': {
            \       'left': [['shortname', 'modified']],
            \       'right': [['lineinfo' ], [ 'percent']],
            \   },
            \   'component_function': {
            \       'shortname': 'LightlineShortName',
            \       'fugitive': 'FugitiveHead',
            \       'gitgutter': 'LightlineGitGutter',
            \   }
            \ }

" Tmux status line
let g:tmuxline_powerline_separators = 0
let g:tmuxline_theme = 'vim_statusline_1'
let g:tmuxline_preset = 'minimal'

" Disable jedi completion and other fancy features
let g:jedi#auto_vim_configuration = 0
let g:jedi#show_call_signatures = 0
let g:jedi#completions_enabled = 0
let g:jedi#goto_command = '<Leader>jj'
let g:jedi#goto_assignments_command = '<Leader>ja'
let g:jedi#goto_stubs_command = '<Leader>js'
let g:jedi#goto_definitions_command = '<Leader>jd'
let g:jedi#rename_command = '<Leader>jr'
let g:jedi#usages_command = '<Leader>ju'

" Enable deoplete completions and hints
if has('nvim')
    let g:deoplete#enable_at_startup = 1
    let g:echodoc_enable_at_startup = 1
endif

" Autocommands {{{1

augroup vimrc
    autocmd!

    " Run pytest in the directory of the current file
    if executable('pytest')
        autocmd Filetype python let b:dispatch = 'pytest -q %:h'
    endif

    " Use two spaces for some languages
    autocmd Filetype ruby,eruby,javascript,html,dockerfile,yaml setl sw=2 sts=2

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
                \   exe "normal g`\"" |
                \ endif

    " Auto-open quickfix
    autocmd QuickFixCmdPost [^l]* cwindow
    autocmd QuickFixCmdPost l* lwindow
augroup END

" Commands and mappings {{{1

" Marks include column
noremap ' `

" Quick shortcuts
nnoremap <silent> <Leader><Leader> :nohlsearch<CR>
nnoremap <silent> <Leader>s :write<CR>:nohlsearch<CR>
nnoremap <silent> <Leader>S :wall<CR>:nohlsearch<CR>
nnoremap <silent> <Leader>q :close<CR>

" Open/close quickfix windows
nnoremap <silent> <Leader>c :copen<CR>
nnoremap <silent> <Leader>C :cclose<CR>
nnoremap <silent> <Leader>l :lopen<CR>
nnoremap <silent> <Leader>L :lclose<CR>

" Paste last yank
noremap <Leader>p "0p
noremap <Leader>P "0P

" Very magic search
nnoremap <Leader>/ /\v

" Custom grep command and mapping
command! -nargs=+ -complete=file -bar Grep silent grep! <args>
nnoremap <Leader>a :Grep<Space>
nnoremap <Leader>A :Grep -w '<C-r><C-w>'

" Buffer search
nnoremap <C-b> :CtrlPBuffer<CR>

" Formatting instead of ex mode
nmap Q <Plug>(ale_fix)

" Cycle between windows
nnoremap <C-j> <C-w>w
nnoremap <C-k> <C-w>W

" Emacs bindings for the command line (see :h emacs-keys)
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
" cnoremap <C-b> <Left>
" cnoremap <C-f> <Right>
cnoremap <C-d> <Del>
" cnoremap <M-b> <S-Left>
" cnoremap <M-f> <S-Right>
" cnoremap <M-p> <Up>
" cnoremap <M-n> <Down>

" Quickfix navigation
nnoremap <silent> <C-Left> :cfirst<CR>
nnoremap <silent> <C-Up> :cprev<CR>
nnoremap <silent> <C-Down> :cnext<CR>
nnoremap <silent> <C-Right> :clast<CR>

" Fix C-c not triggering autocmds
inoremap <C-c> <Esc>

" Easy exit from terminal mode
tnoremap <Esc> <C-\><C-n>

" Delete the buffer and preserve the window
nnoremap <Leader>bd :Sayonara!<CR>

" Dispatch mappings
nnoremap <Leader>d :Dispatch<CR>
nnoremap <Leader>D :Dispatch<Space>

" ALE LSP mappings
nmap <Leader>gd <Plug>(ale_go_to_definition)
nmap <Leader>gt <Plug>(ale_go_to_type_definition)
nmap <Leader>gh <Plug>(ale_hover)
nmap <Leader>gk <Plug>(ale_documentation)
nmap <Leader>gf <Plug>(ale_find_references)

" Package management helpers
command! PackUpdate call minpac#update()
command! PackClean call minpac#clean()
