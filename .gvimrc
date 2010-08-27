" Color scheme
colorscheme desert256

" Make the window higher
set lines=40

" Font settings
if has("gui_macvim")
    set guifont=DejaVu\ Sans\ Mono:h13
else
    set guifont=Inconsolata\ 12
endif

" No toolbar
set guioptions-=T
