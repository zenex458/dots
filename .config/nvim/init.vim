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
set guicursor=i:block-iCursor
set guicursor+=i:blinkon10
set guicursor+=n-v-c:blinkon10
set statusline+=\%{toupper(g:currentmode[mode()])} 
set statusline+=%<%f%m\ \ \ %=\ %R%H%W\ %l/%L:%c\ %p%% "add %Y for file type
"%R=readonly, %H=helpbuffer, %W=preview window
colorscheme dalton
map <leader>sp :setlocal spell! spelllang=en_gb<CR>
tnoremap <Esc> <C-\><C-n>
vmap <C-c> "+y
