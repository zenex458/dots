" vim: fdm=marker foldenable sw=4 ts=4 sts=4
" "zo" opens, "zc" closes, "zn" disable
" {{{ Plugins
call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')
Plug 'norcalli/nvim-colorizer.lua'
Plug 'vimwiki/vimwiki'
Plug 'vim-syntastic/syntastic'
Plug 'jiangmiao/auto-pairs'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neovimhaskell/haskell-vim'
"Plug 'OmniSharp/omnisharp-vim'
"Plug 'luochen1990/rainbow'
call plug#end()
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
let g:rainbow_active = 1
let mapleader = " "
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:OmniSharp_server_use_net6 = 1
" }}}
" {{{ options
let g:currentmode={
       \ 'n'  : '[N] ',
       \ 'v'  : '[V] ',
       \ 'V'  : '[VLine] ',
       \ "\<C-V>" : '[VBlock] ',
       \ 'i'  : '[I] ',
       \ 'R'  : '[R] ',
       \ 'Rv' : '[V·Replace] ',
       \ 'c'  : '[Command] ',
       \}
syntax on
filetype plugin indent on
"set laststatus=0
set ruler
set ignorecase
set smartcase
set smartindent
set autoindent
set cursorline
set title
set cursorcolumn
set showcmd
set showmatch
set hlsearch
set title
set nocompatible
set wildmode=longest,list,full
set clipboard+=unnamedplus
set termguicolors
set noshowmode
"set guicursor=n-v-c:hor50-Cursor
"set guicursor+=i:hor50-Cursor
set statusline+=\%{toupper(g:currentmode[mode()])} 
set statusline+=%<%f%m\ \ \ %=\ %R%H%W\ %l/%L:%c\ %p%% "add %Y for file type
set guicursor=i:block-iCursor
set guicursor+=i:blinkon100
set guicursor+=n-v-c:blinkon100
"set mouse=a
set number
"set linebreak

colorscheme dalton
" }}}
" {{{ keybinds
map <leader>r :%s/
map <leader>+ <C-w>+
map <leader>- <C-w>-
map <leader>= <C-w>=
map <leader>/ :noh<CR>
map <leader>sp :setlocal spell! spelllang=en_gb<CR>
tnoremap <Esc> <C-\><C-n>
vmap <C-c> "+y
map <leader>oe :bro ol<CR>
lua require'colorizer'.setup()
" }}}
