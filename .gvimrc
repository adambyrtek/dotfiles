" Color scheme
colorscheme desert256

" Make the window higher
set lines=40

" Font settings
if has("gui_macvim")
    set guifont=DejaVu\ Sans\ Mono:h13
else
    set guifont=Droid\ Sans\ Mono\ 10
endif

" No toolbar
set guioptions-=T

" No popups
set guioptions+=c
