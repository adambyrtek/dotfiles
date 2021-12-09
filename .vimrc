" vim: foldmethod=marker

" Packages {{{1

" Use minipac for managing packages
packadd minpac

if exists('g:loaded_minpac')
    call minpac#init()

    " Async completions
    call minpac#add('Shougo/context_filetype.vim')
    call minpac#add('Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'})
    call minpac#add('Shougo/neco-vim')
    " call minpac#add('deoplete-plugins/deoplete-go', {'do': 'make'})
    call minpac#add('deoplete-plugins/deoplete-jedi')
    call minpac#add('deoplete-plugins/deoplete-lsp')

    call minpac#add('Vimjas/vim-python-pep8-indent')
    call minpac#add('airblade/vim-gitgutter')
    call minpac#add('ap/vim-css-color')
    call minpac#add('chaoren/vim-wordmotion')
    call minpac#add('ctrlpvim/ctrlp.vim')
    call minpac#add('davidhalter/jedi-vim')
    call minpac#add('hashivim/vim-terraform')
    call minpac#add('itchyny/lightline.vim')
    call minpac#add('justinmk/vim-dirvish')
    call minpac#add('kana/vim-textobj-entire')
    call minpac#add('kana/vim-textobj-indent')
    call minpac#add('kana/vim-textobj-line')
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
    call minpac#add('tpope/vim-sleuth')
    call minpac#add('tpope/vim-surround')
    call minpac#add('tpope/vim-unimpaired')
    call minpac#add('w0rp/ale')
    call minpac#add('wellle/targets.vim')
endif


" Appearance {{{1

" Assume the terminal supports true color
set termguicolors

" Gruvbox with optional italics
let g:gruvbox_italic = 1
colorscheme gruvbox

" Switch to the dark version (has to be done after loading the scheme)
set background=dark


" Options {{{1

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

" Case insensitive search...
set ignorecase

" ...except when a capital letter is used
set smartcase

" Use spaces instead of tabs
set expandtab

" Number of spaces to use for indent
set shiftwidth=4

" Tab in insert mode indents with spaces
set smarttab

" Indent new lines as previous line
set autoindent

" Incremental search
set incsearch

" Incremental substitution
if has('nvim')
    set inccommand=split
endif

" Highlight search results
set hlsearch

" Allow backspace to delete everything
set backspace=indent,eol,start

" Always show status
set laststatus=2

" Keep some space around the edges when scrolling
set scrolloff=8 sidescrolloff=8

" Modified buffers can be hidden
set hidden

" Show indicator on wrapped lines
set showbreak=↪

" Show cursor position on the bottom right
set ruler

" Command mode completion menu
set wildmenu

" Complete longest prefix first
set wildmode=longest:full,full

" Case insensitive completion
set wildignorecase

" Better insert mode completion
set completeopt=longest,menuone,noselect

" Do not show completion messages on the command line
set shortmess+=c

" Show matching parenthesis
set showmatch

" No swap files
set noswapfile

" Always display special characters
set list

" Special characters to show
set listchars=tab:»·,extends:›,precedes:‹,nbsp:·,trail:·

" Default global replace (deprecated)
set gdefault

" Longer history
set history=10000

" Use one space instead of two when joining sentences
set nojoinspaces

" Remove comment character when joining comment lines
set formatoptions+=j

" Reload file when changed (and not modified in Vim)
set autoread

" Shorter timeout after Esc in classic Vim
if !has('nvim')
    set ttimeout
    set ttimeoutlen=50
endif

" Shorter cursor hold timeout (e.g. for Git gutter)
set updatetime=250

" Remove include files from path
set path-=/usr/include

" Avoid completing include files
set complete-=i

" Magic to make true color work in tmux
if !has('nvim')
    set t_8f=[38;2;%lu;%lu;%lum
    set t_8b=[48;2;%lu;%lu;%lum
endif

" Use ag or rg instead of grep (if available)
if executable('rg')
    set grepprg=rg\ --vimgrep\ --sort=path
elseif executable('ag')
    set grepprg=ag\ --vimgrep
endif

" Support column numbers in grep output
set grepformat^=%f:%l:%c:%m

" More intuitive splits
set splitright splitbelow

" Disable modelines for security reasons
set nomodeline

" Always show the gutter
set signcolumn=yes


" Plugins settings {{{1

" Change leader to work better with different layouts
let mapleader = ','

" Always start search from cwd
let g:ctrlp_working_path_mode = 'rw'

" Include current file in search results
let g:ctrlp_match_current_file = 1

" Use external commands for building CtrlP index (if available)
if executable('rg')
    let g:ctrlp_user_command = 'rg %s --files --color=never'
    let g:ctrlp_use_caching = 0
elseif executable('ag')
    let g:ctrlp_user_command = 'ag %s --nocolor -g ""'
    let g:ctrlp_use_caching = 0
endif

" Mapping to delete buffers
let g:ctrlp_prompt_mappings = {'PrtDeleteEnt()': ['<c-k>']}

" Reuse windows from selected plugins
let g:ctrlp_reuse_window = 'dirvish'

" Avoid jumping to open windows
let g:ctrlp_switch_buffer = ''

" Prevent netrw from loading
let loaded_netrwPlugin = 1

" Advanced word motions
let g:wordmotion_prefix = ','

" ALE linters
let g:ale_linters = {}

" ALE fixers
let g:ale_fixers = {
            \     '*': ['remove_trailing_lines', 'trim_whitespace'],
            \     'python': ['isort', 'black'],
            \     'javascript': ['eslint'],
            \     'json': ['jq'],
            \     'terraform': ['terraform'],
            \     'yaml': ['yamlfix'],
            \ }

" Linter name in the error message
let g:ale_echo_msg_format = '[%linter%] %code: %%s'

" Avoid LSP inception
let g:ale_disable_lsp = 1

" Neovim should use system Python (not one from venv)
if has('nvim')
    let g:python_host_prog = '/usr/bin/python2'
    let g:python3_host_prog = '/usr/bin/python3'
endif

" Custom Jedi configuration
let g:jedi#auto_vim_configuration = 0
let g:jedi#show_call_signatures = 0
let g:jedi#completions_enabled = 0
let g:jedi#goto_command = '<C-]>'
let g:jedi#goto_assignments_command = '<Leader>ja'
let g:jedi#goto_stubs_command = '<Leader>js'
let g:jedi#goto_definitions_command = '<Leader>jd'
let g:jedi#rename_command = '<Leader>jr'
let g:jedi#usages_command = '<Leader>ju'

" Enable Deoplete completions
if has('nvim')
    let g:deoplete#enable_at_startup = 1
endif

" Allow code embedded in Markdown
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh']


" Status line {{{1

" Git status
function! LightlineGitGutter() abort
    if &buftype ==# '' && exists('*GitGutterGetHunkSummary')
        let [l:a, l:m, l:r] = GitGutterGetHunkSummary()
        if l:a + l:m + l:r > 0
            return printf('+%d ~%d -%d', l:a, l:m, l:r)
        end
    end
    return ''
endfunction

" Shorten file name
function! LightlineShortName() abort
    if &filetype ==# 'qf'
        return exists('w:quickfix_title') ? w:quickfix_title : 'quickfix'
    else
        let l:shortname = expand('%:~:.')
        return l:shortname != '' ? l:shortname : expand('%:~')
    end
endfunction

" Number of quickfix items
function! LightlineQuickfix() abort
    let l:count = len(getqflist())
    if l:count == 0
        return ''
    endif

    let l:current_idx = get(getqflist({'idx': 0}), 'idx', 0)
    let l:count_valid = len(filter(getqflist(), 'v:val.valid'))
    if l:count == l:count_valid
        return printf('qf %d/%d', l:current_idx, l:count)
    else
        return printf('qf %d/%d(%d)', l:current_idx, l:count, l:count_valid)
    endif
endfunction

" Lightline configuration
let g:lightline = {
            \     'colorscheme': 'gruvbox',
            \     'active': {
            \         'left': [['mode', 'paste'], ['shortname', 'modified'], ['fugitive', 'gitgutter' ]],
            \         'right': [['lineinfo'], ['quickfix', 'percent'], ['filetype', 'spell', 'readonly']],
            \     },
            \     'inactive': {
            \         'left': [['shortname', 'modified']],
            \         'right': [['lineinfo' ], [ 'percent']],
            \     },
            \     'component_function': {
            \         'shortname': 'LightlineShortName',
            \         'fugitive': 'FugitiveHead',
            \         'gitgutter': 'LightlineGitGutter',
            \         'quickfix': 'LightlineQuickfix',
            \     }
            \ }


" Commands and autocommands {{{1

" Package management helpers
command! PackUpdate call minpac#update()
command! PackClean call minpac#clean()

" Silent grep that doesn't open the first match
command! -nargs=+ -complete=file -bar Grep silent grep! <args>

augroup vimrc
    autocmd!

    " Run pytest in the directory of the current file
    if executable('pytest')
        autocmd Filetype python let b:dispatch = 'pytest -q %:h'
    endif

    " Spell check Git commit messages
    autocmd Filetype gitcommit setl spell spl=en

    " Prevent overriding of the global mapping
    autocmd FileType dirvish silent! unmap <buffer> <C-p>

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

    " Auto-open quickfix
    autocmd QuickFixCmdPost [^l]* cwindow
    autocmd QuickFixCmdPost l* lwindow
augroup END


" Key mappings {{{1

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

" Search using more standard regexp syntax
nnoremap <Leader>/ /\v
nnoremap <Leader>? ?\v

" Grep shortcuts
nnoremap <Leader>a :Grep<Space>
nnoremap <Leader>A :Grep -w '<C-r><C-w>'

" Buffer search
nnoremap <C-b> :CtrlPBuffer<CR>

" Formatting instead of ex mode
nmap Q <Plug>(ale_fix)

" Dwm style window navigation
nnoremap <C-j> <C-w>w
nnoremap <C-k> <C-w>W

" Emacs bindings for the command line (see :h emacs-keys)
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-d> <Del>

" Quickfix navigation
nnoremap <silent> <C-Up> :cprev<CR>
nnoremap <silent> <C-Down> :cnext<CR>

" Fix C-c not triggering autocmds
inoremap <C-c> <Esc>

" Easy exit from terminal mode
tnoremap <Esc> <C-\><C-n>

" Delete the buffer and preserve the window
nnoremap <Leader>bd :Sayonara!<CR>

" Dispatch mappings
nnoremap <Leader>d :Dispatch<CR>
nnoremap <Leader>D :Dispatch<Space>
nnoremap <Leader>m :Make<CR>
nnoremap <Leader>M :Make<Space>

" Repeat last edit over visual selection
xmap <silent> . :normal .<CR>
