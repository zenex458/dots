let mapleader = " "
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
"set guicursor=i:bloack-iCursor
"set guicursor+=i:blinkon100
"set guicursor=n-v-c:hor50-Curosr
"set guicursor+=n-v-c:blinkon100

set guicursor=i:hor50-Cursor
set guicursor+=i:blinkon100
set guicursor+=n-v-c:blinkon100


"set guicursor=n-v-c:hor50-Cursor
"set guicursor+=i:hor50-Cursor
set statusline+=\%{toupper(g:currentmode[mode()])} 
set statusline+=%<%f%m\ \ \ %=\ %R%H%W\ %l/%L:%c\ %p%% "[%n] %Y
"set guicursor=i:block-iCursor
"set guicursor+=i:blinkon100
"set guicursor+=n-v-c:blinkon100
"set mouse=a
"set linebreak
set rnu nu
colorscheme morest

inoremap <expr> <Tab> pumvisible() ? '<C-n>' :                                                                                                                    
\ getline('.')[col('.')-2] =~# '[[:alnum:].-_#$]' ? '<C-x><C-o>' : '<Tab>'
map <leader>r :%s/
map <leader>+ <C-w>+
map <leader>- <C-w>-
map <leader>= <C-w>=
map <leader>/ :noh<CR>
map <leader>sp :setlocal spell! spelllang=en_gb<CR>
tnoremap <Esc> <C-\><C-n>
vmap <C-c> "+y
map <leader>oe :bro ol<CR>
map <leader>vh :VimwikiAll2HTML<CR>
