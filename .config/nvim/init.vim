syntax on
filetype plugin indent on
set ruler
set ignorecase
set smartcase
set smartindent
set autoindent
"set number
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
set guicursor=n-v-c:hor50-Cursor
set guicursor+=i:block
"set guicursor+=i:ver100-iCursor
"set mouse=a

colorscheme dalton

"highlight Cursor guifg=steelblue guibg=red
"highlight iCursor guifg=steelblue guibg=red

let mapleader = " "
let NERDTreeShowHidden= 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:rainbow_active = 1
let g:syntastic_cs_checkers = ['code_checker']

map <leader>t :NERDTreeToggle<CR>
map <leader>r :%s/
map <leader>+ <C-w>+
map <leader>- <C-w>-
map <leader>= <C-w>=
map <leader>/ :noh<CR>
map <leader>sp :setlocal spell! spelllang=en_gb<CR>
tnoremap <Esc> <C-\><C-n>
vmap <C-c> "+y

call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')
Plug 'jiangmiao/auto-pairs'
Plug 'preservim/nerdtree'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'vimwiki/vimwiki'
Plug 'luochen1990/rainbow'
Plug 'vim-syntastic/syntastic'
call plug#end()
lua require'colorizer'.setup()
