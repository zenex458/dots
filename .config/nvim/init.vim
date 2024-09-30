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
set wildmode=longest,list,full
set clipboard+=unnamedplus
set termguicolors
set noshowmode
set guicursor=n-v-c:hor50-Cursor
set guicursor+=i:hor50-Cursor
set statusline+=\%{toupper(g:currentmode[mode()])} 
set statusline+=%<%f%m\ \ \ %=\ %R%H%W\ %l/%L:%c\ %p%% "add %Y for file type
"%R=readonly, %H=helpbuffer, %W=preview window
"set linebreak
colorscheme dalton
map <leader>sp :setlocal spell! spelllang=en_gb<CR>
vmap <C-c> "+y
