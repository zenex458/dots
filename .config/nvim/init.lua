vim.wo.number = true
vim.wo.relativenumber = true
vim.o.termguicolors = true
vim.g.mapleader = " "
vim.o.statusline = "%<%f%m   %= %R%H%W %l/%L:%c %p%% "
vim.o.guicursor = "i:hor50-Cursor,i:blinkon100,n-v-c:blinkon100"

vim.o.clipboard = "unnamedplus"
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	"hrsh7th/nvim-cmp", -- Autocompletion plugin,
	"hrsh7th/cmp-nvim-lsp", -- LSP source for nvim-cmp,
	"saadparwaiz1/cmp_luasnip", -- Snippets source for nvim-cmp
	"nvim-lua/popup.nvim",
	"stevearc/conform.nvim",
	"nvim-lua/plenary.nvim",
	"L3MON4D3/LuaSnip", -- Snippets plugin
	"nvim-treesitter/nvim-treesitter",
	"neovim/nvim-lspconfig",
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.5",
		dependencies = { "nvim-lua/plenary.nvim" },
	},

	{
		"fenetikm/falcon",
		config = function()
			vim.cmd([[colorscheme falcon]])
		end,
	},
})

require("telescope").setup({
	defaults = {
		file_ignore_patterns = { ".git/[^h]" },
	},
	pickers = {
		find_files = {
			hidden = true,
		},
	},
})
require("nvim-treesitter.configs").setup({
	-- A list of parser names, or "all" (the five listed parsers should always be installed)
	ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "bash" },

	-- Install parsers synchronously (only applied to `ensure_installed`)
	sync_install = false,

	-- Automatically install missing parsers when entering buffer
	-- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
	auto_install = true,
	---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
	-- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

	highlight = {
		enable = true,
		-- Setting this to true will run `:h syntax` and tree-sitter at the same time.
		-- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
		-- Using this option may slow down your editor, and you may see some duplicate highlights.
		-- Instead of true it can also be a list of languages
		additional_vim_regex_highlighting = false,
	},
})

require("conform").setup({
	format_on_save = {
		-- These options will be passed to conform.format()
		timeout_ms = 500,
		lsp_fallback = true,
	},
	formatters_by_ft = {
		lua = { "stylua" },
		csharp = { "csharpier" },
		c = { "astyle" },
		sh = { "shfmt" },
	},
})

vim.api.nvim_create_autocmd("BufWritePre", {
	pattern = "*",
	callback = function(args)
		require("conform").format({ bufnr = args.buf })
	end,
})
vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
local lspconfig = require("lspconfig")

local capabilities = require("cmp_nvim_lsp").default_capabilities()
local luasnip = require("luasnip")

local servers = {
	"lua_ls",
	"csharp_ls",
	"clangd",
	"bashls",
}

for _, lsp in ipairs(servers) do
	lspconfig[lsp].setup({
		-- on_attach = my_custom_on_attach,
		capabilities = capabilities,
	})
end

local cmp = require("cmp")
cmp.setup({
	formatting = {

		fields = { "abbr", "menu" },
		format = function(entry, vim_item)
			vim_item.menu = ({
				nvim_lsp = "[LSP]",
				luasnip = "[Snippet]",
				buffer = "[Buffer]",
				path = "[Path]",
			})[entry.source.name]
			return vim_item
		end,
	},
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	mapping = cmp.mapping.preset.insert({
		["<C-u>"] = cmp.mapping.scroll_docs(-4), -- Up
		["<C-d>"] = cmp.mapping.scroll_docs(4), -- Down
		["<C-j>"] = cmp.mapping.select_next_item(),
		["<C-k>"] = cmp.mapping.select_prev_item(),
		-- C-b (back) C-f (forward) for snippet placeholder navigation.
		["<C-Space>"] = cmp.mapping.complete(),
		["<CR>"] = cmp.mapping.confirm({
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		}),
		["<Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif luasnip.expand_or_jumpable() then
				luasnip.expand_or_jump()
			else
				fallback()
			end
		end, { "i", "s" }),
	}),

	window = {
		documentation = {
			border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
		},
	},
	experimental = {
		ghost_text = false,
		native_menu = false,
	},
	sources = {
		{ name = "nvim_lsp" },

		{ name = "luasnip" },
		{ name = "buffer" },
		{ name = "path" },
	},

	["<S-Tab>"] = cmp.mapping(function(fallback)
		if cmp.visible() then
			cmp.select_prev_item()
		elseif luasnip.jumpable(-1) then
			luasnip.jump(-1)
		else
			fallback()
		end
	end, { "i", "s" }),
})
local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>ff", builtin.find_files, {})
vim.keymap.set("n", "<leader>fg", builtin.live_grep, {})
vim.keymap.set("n", "<leader>fb", builtin.buffers, {})
vim.keymap.set("n", "<leader>fh", builtin.help_tags, {})
vim.keymap.set("i", "jk", [[<ESC>]])
