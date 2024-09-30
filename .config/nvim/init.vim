" vim: fdm=marker foldenable sw=4 ts=4 sts=4
" "zo" opens, "zc" closes, "zn" disable
" {{{ Plugins
call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')
Plug 'norcalli/nvim-colorizer.lua'
Plug 'vimwiki/vimwiki'
Plug 'jiangmiao/auto-pairs'
"Plug 'luochen1990/rainbow'
"c#
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'


call plug#end()
let mapleader = " "
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
set laststatus=0
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
"set noshowmode
"set guicursor=n-v-c:hor50-Cursor
"set guicursor+=i:hor50-Cursor
set statusline+=\%{toupper(g:currentmode[mode()])} 
set statusline+=%<%f%m\ \ \ %=\ %R%H%W\ %l/%L:%c\ %p%% "[%n] %Y
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
map <leader>vh :VimwikiAll2HTML<CR>
lua require'colorizer'.setup()
" }}}
" {{{ lua
lua << EOF
local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

local lsp_flags = {
  -- This is the default in Nvim 0.7+
  debounce_text_changes = 150,
}

require('lspconfig')['tsserver'].setup{
  on_attach = on_attach,
  flags = lsp_flags,
}

local pid = vim.fn.getpid()
local omnisharp_bin = "omnisharp"

require('lspconfig')['omnisharp'].setup{
  cmd = { omnisharp_bin, "--languageserver" , "--hostPID", tostring(pid) },
  on_attach = on_attach,
  capabilities = capabilities
}


local cmp = require'cmp'

cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
      -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'vsnip' }, -- For vsnip users.
    -- { name = 'luasnip' }, -- For luasnip users.
    -- { name = 'ultisnips' }, -- For ultisnips users.
    -- { name = 'snippy' }, -- For snippy users.
  }, {
    { name = 'buffer' },
  })
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources({
    { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
  }, {
    { name = 'buffer' },
  })
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})
EOF
" }}}
