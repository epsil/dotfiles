set noswapfile
set number
set ruler
set laststatus=2
set showcmd
set ignorecase
set incsearch
set hlsearch
"set textwidth=70
set shiftwidth=4
set nojoinspaces
set expandtab
set cursorline
set mouse=a
set wrap
set linebreak
syntax on

set encoding=utf-8
"set guifont=Fixedsys\ Excelsior\ 14
"set guifont=Consolas\ 12
"set guifont=Ubuntu\ Mono\ 14
"set guifont=Hack\ 12
"set guifont=Ubuntu\ Mono\ 14
set guifont=Ubuntu\ Mono\ 16
"set guifont=Hack\ 14
"set guifont=Source\ Code\ Pro\ 16
"set guifont=DejaVu\ Sans\ Mono\ 16

set autoindent
set smartindent

"map <Tab> <Esc>
"imap <Tab> <Esc>
"vmap <Tab> <Esc>

"map ¤ $
map æ [{
map Æ }
map ø `
map Ø @
map å [[
map æ []
map Å {

map <F1> <nop>
nmap <F1> <nop>
imap <F1> <nop>
map <F5> :e!<CR>
imap <F5> :e!<CR>
vmap <F5> :e!<CR>
map <F10> <C-t>
map <F11> <C-]>
map  <F12> :nohlsearch<CR>
imap <F12> <ESC>:nohlsearch<CR>a
vmap <F12> <ESC>:nohlsearch<CR>gv

map <M-q> !}fmt<CR>
imap <M-q> <Esc>!}fmt<CR>i

" autocmd BufAdd,BufNewFile,BufRead * nested tab sball
" autocmd BufWritePre * :%s/\s\+$//e

" Set an orange cursor in insert mode, and a red cursor otherwise.
" Works at least for xterm and rxvt terminals.
" Does not work for gnome terminal, konsole, xfce4-terminal.
" if &term =~ "xterm\\|rxvt"
"   :silent !echo -ne "\033]12;red\007"
"   let &t_SI = "\033]12;green\007"
"   let &t_EI = "\033]12;blue\007"
"   autocmd VimLeave * :!echo -ne "\033]12;red\007"
" endif
