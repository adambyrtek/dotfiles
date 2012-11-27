" vim: foldmethod=marker

" Pathogen {{{1

" Initialize Pathogen, has to be at the top
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" Appearance {{{1

" Background autodetection often fails
set background=dark

" Default color schemes
if has("gui_running")
  colorscheme solarized
  "colorscheme zenburn
else
  if &t_Co >= 256
    colorscheme wombat256mod
    "colorscheme desert256
  else
    " Looks decent with default terminal colors
    colorscheme elflord
    " Use with Solarized terminal colors only
    "colorscheme solarized
  endif
endif

" Font settings for GUI
if has("gui_macvim")
  set guifont=Menlo\ Regular:h13
else
  set guifont=Droid\ Sans\ Mono\ 10
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

" Wrap lines on screen at whitespace (requires nolist)
set linebreak

" Display incomplete wrapped lines at the end of the screen
set display=lastline

" Don't distinguish case in search and completion
set ignorecase

" ...except when a capital letter is used
set smartcase

" Tab indents and Backspace deindents
set smarttab

" Use spaces instead of tabs
set expandtab

" Number of spaces to use for indent
set shiftwidth=2

" Number of spaces to use for Tab in insert mode
set softtabstop=2

" Incremental search
set incsearch

" Allow backspace to delete everything
set backspace=indent,eol,start

" Indent new lines as previous line
set autoindent

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

" Status line (superseded by Powerline)
set statusline=%.50f\ %h%#ErrorMsg#%m%*%r%y\ %=\ %(%l,%c%V\ \(%P\)%)

" Show matching parenthesis
set showmatch

" No backups
set nobackup

" No swap files
set noswapfile

" Always display special characters
set list

" Special characters to show
set listchars=tab:>·,trail:·

" Grep options
set grepprg=grep\ -HERn\ $*\ /dev/null

" No GUI toolbar
set guioptions-=T

" No GUI popups
set guioptions+=c

" Default global replace
set gdefault

" Longer history
set history=1000

" Use one space instead of two when joining sentences
set nojoinspaces

" Variables {{{1

" Leader key bindings
let mapleader = '\'
let maplocalleader = ','

" Show relative paths in buffer explorer
let g:bufExplorerShowRelativePath = 1

" Prefer symmetric encryption
let g:GPGPreferSymmetric = 1

" Using Esc causes problems on some platforms
let g:CommandTCancelMap = '<C-c>'

" Case sensitive matching when pattern contains uppercase characters
let g:CommandTSmartCaseMatching = 1

" Vimwiki default browser
let g:vimwiki_browsers = ['xdg-open']

" Ack binary on Debian/Ubuntu
let g:ackprg = 'ack-grep -H --nocolor --nogroup --column'

" Pyflakes won't clutter the quickfix
let g:pyflakes_use_quickfix = 0

" Use signs for quickfix and location
let g:quickfixsigns_classes = ['qfl', 'loc']

" Surround comments with spaces
let NERDSpaceDelims = 1

" Autocommands {{{1

augroup vimrc
  au!

  " Highlight the cursor line in the current buffer
  autocmd VimEnter,WinEnter * set cursorline
  autocmd WinLeave * set nocursorline

  " Additional filetype mappings
  autocmd BufNewFile,BufRead mail.google.com.* set filetype=mail
  autocmd BufNewFile,BufRead *.erb set filetype=eruby
  autocmd BufNewFile,BufRead *.md set filetype=markdown

  " Mail
  autocmd FileType mail setl spell
  autocmd FileType mail setl wrap

  " LaTeX
  autocmd FileType tex compiler tex

  " PHP
  autocmd FileType php setl makeprg=php5\ -l\ %
  autocmd FileType php setl errorformat=%m\ in\ %f\ on\ line\ %l

  " Python
  " http://blog.sontek.net/2008/05/11/python-with-a-modular-ide-vim/
  autocmd FileType python setl makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"
  autocmd FileType python setl errorformat=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m

  " Ruby
  autocmd FileType ruby setl tags+=$HOME/.gems/tags

  " Automatically close Fugitive buffers
  autocmd BufReadPost fugitive://* set bufhidden=delete

  " Go to last known position
  autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     exe "normal g`\"" |
        \ endif

augroup END

" Keyboard mappings {{{1

" Paste in visual mode shouldn't replace the default register
vnoremap p "_xP

" Better use of Q instead of ex mode
nnoremap Q gqap
vnoremap Q gq

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

" Enter accepts the current completion
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"

" Tab in normal mode
nnoremap <Tab> %

" Leader shortcuts to frequent actions
nnoremap <Leader><Leader> <Nop>
nnoremap <Leader>a :BA<CR>
nnoremap <Leader>A :A<CR>
nnoremap <Leader>q :botright copen<CR>
nnoremap <Leader>Q :cex []<CR>:cw<CR>
nnoremap <Leader>d :BD<CR>
nnoremap <Leader>m :w<CR>:make<CR>:cw<CR>
nnoremap <Leader>M :!ctags<CR>
nnoremap <Leader>b :CommandTBuffer<CR>
nnoremap <Leader>t :CommandT<CR>
nnoremap <Leader>T :TagbarToggle<CR>
nnoremap <Leader>n :NERDTreeToggle<CR>
nnoremap <Leader>N :NERDTreeFind<CR>
nnoremap <Leader>e :BufExplorer<CR>
nnoremap <Leader>v `[V`]
nnoremap <Leader>p :CtrlP<CR>

" Edit file based on current file or directory
nnoremap <Leader>o :e <C-R>=expand('%:h')<CR>/
nnoremap <Leader>O :e <C-R>=expand('%')<CR>

" Diff against the version on disk.
nnoremap <Leader>u :w !diff -u % -<CR>

" Write buffer using sudo
command! W w !sudo tee % > /dev/null

" Quit all
command! Q qall

" Toggles
nnoremap <Leader>gh :set hlsearch!<CR>:set hlsearch?<CR>
nnoremap <Leader>gp :set paste!<CR>:set paste?<CR>
nnoremap <Leader>gn :set number!<CR>:set number?<CR>
nnoremap <Leader>gc :set cursorline<CR>:set cursorline?<CR>
nnoremap <Leader>gl :setl list!<CR>:setl list?<CR>
nnoremap <Leader>gs :setl spell!<CR>:setl spell?<CR>
nnoremap <Leader>gw :setl wrap!<CR>:setl wrap?<CR>
nnoremap <Leader>go :let &scrolloff=999-&scrolloff<CR>:set scrolloff?<CR>
nnoremap <Leader>gb :let &background = ( &background == "dark"? "light" : "dark" )<CR>:set background?<CR>

" Emacs bindings for the command line
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-d> <Del>

" Fast switching between windows
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l

" Forget about the arrow keys
nnoremap <Up> <Nop>
nnoremap <Down> <Nop>
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>
inoremap <Up> <Nop>
inoremap <Down> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>

" Semicolon to enter the command mode
nnoremap ; :
vnoremap ; :

" Navigation
nnoremap <silent> [Q :cfirst<CR>
nnoremap <silent> ]Q :clast<CR>
nnoremap <silent> [q :cprevious<CR>
nnoremap <silent> ]q :cnext<CR>
nnoremap <silent> [T :tfirst<CR>
nnoremap <silent> ]T :tlast<CR>
nnoremap <silent> [t :tprevious<CR>
nnoremap <silent> ]t :tnext<CR>
nnoremap <silent> [B :bfirst<CR>
nnoremap <silent> ]B :blast<CR>
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
