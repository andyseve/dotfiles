local plugins = require("core.user").plugins
local utils = require("core.utils")

Spec = {
	-- UI
	{
		"akinsho/bufferline.nvim",
		enabled = plugins.bufferline,
		lazy = false,
		dependencies = "nvim-tree/nvim-web-devicons",
		config = function() require("config.bufferline") end,
	},
	{
		"nvim-lualine/lualine.nvim",
		enabled = plugins.lualine,
		lazy = false,
		config = function() require("config.lualine") end,
	},
	{
		"rcarriga/nvim-notify",
		enabled = plugins.notify,
		lazy = true,
		config = function() require("config.notify") end,
	},
	{
		"folke/noice.nvim",
		enabled = plugins.noice,
		lazy = true,
		dependencies = {
			"MunifTanjim/nui.nvim",
			{ "rcarriga/nvim-notify", enabled = plugins.notify },
		},
		event = "VeryLazy",
		config = function() require("config.noice") end,
	},
	{
		"lukas-reineke/indent-blankline.nvim",
		enabled = plugins.blankline,
		lazy = true,
		event = "VeryLazy",
		config = function() require("config.blankline") end,
	},
	{
		"folke/which-key.nvim",
		enabled = plugins.whichkey,
		lazy = false,
		config = function() require("config.whichkey") end,
	},
	{
		"folke/zen-mode.nvim",
		enabled = plugins.zenmode,
		lazy = true,
		config = function() require("config.zenmode") end,
	},
	{
		"lewis6991/gitsigns.nvim",
		enabled = plugins.gitsigns,
		lazy = true,
		config = function() require("config.gitsigns") end,
	},
	{
		"folke/trouble.nvim",
		enabled = plugins.trouble,
		lazy = true,
		dependencies = "nvim-tree/nvim-web-devicons",
		cmd = "TroubleToggle",
		init = function() require("setup.trouble") end,
		config = function() require("config.trouble") end,
	},
	{
		"folke/twilight.nvim",
		enabled = plugins.twilight,
		lazy = false,
		config = function() require("config.twilight") end,
	},
	-- Themes
	{
		"Tsuzat/NeoSolarized.nvim",
		name = "solarized",
		lazy = true,
		enabled = plugins.solarized,
	},
	{
		"navarasu/onedark.nvim",
		lazy = true,
		enabled = plugins.onedark,
	},
	{
		"RRethy/nvim-base16",
		lazy = true,
		enabled = plugins.base16,
	},
	{
		"catppuccin/nvim",
		name = "catppuccin",
		lazy = true,
		enabled = plugins.catppuccin,
		config = function() require("config.themes") end,
		-- Use catppuccin as a generator
	},
	-- Search and files
	{
		"nvim-telescope/telescope.nvim",
		enabled = plugins.telescope,
		lazy = true,
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-file-browser.nvim",
			"tsakirist/telescope-lazy.nvim",
			"ghassan0/telescope-glyph.nvim",
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
			{ "kelly-lin/telescope-ag",                   enabled = utils.is_exe("ag") },
			{ "debugloop/telescope-undo.nvim",            enabled = not plugins.undotree },
			{ "fhill2/telescope-ultisnips.nvim",          enabled = plugins.ultisnips, },
			{ "benfowler/telescope-luasnip.nvim",         enabled = plugins.luasnip, },
		},
		cmd = 'Telescope',
		init = function() require("setup.telescope") end,
		config = function() require("config.telescope") end,
	},
	{
		"nvim-tree/nvim-tree.lua",
		dependencies = "nvim-tree/nvim-web-devicons",
		cmd = { "NvimTreeToggle", "NvimTreeOpen", "NvimTreeFocus", "NvimTreeFindFileToggle" },
		event = "User DirOpened",
		init = function() require("setup.nvimtree") end,
		config = function() require("config.nvimtree") end,
	},
	{
		"mbbill/undotree",
		enabled = plugins.undotree,
		lazy = true,
		cmd = 'UndotreeToggle',
		init = function() require("setup.undotree") end,
		config = function() require("config.undotree") end,
	},
	{
		"sindrets/diffview.nvim",
		enabled = plugins.diffview,
		lazy = true,
		dependencies = "nvim-lua/plenary.nvim",
		cmd = { 'DiffViewOpen', 'DiffviewFileHistory' },
		config = function() require("config.diffview") end,
	},
	-- Syntax
	{
		"windwp/nvim-autopairs",
		enabled = plugins.autopairs,
		lazy = true,
		event = { 'InsertEnter' },
		config = function() require("config.autopairs") end,
	},
	{
		"andymass/vim-matchup",
		enabled = plugins.matchup,
		lazy = true,
		event = { 'BufWinEnter' },
	},
	{
		"kylechui/nvim-surround",
		enabled = plugins.surround,
		lazy = true,
		event = { 'BufWinEnter' },
		config = function() require("config.surround") end,
	},
	{
		"norcalli/nvim-colorizer.lua",
		enabled = plugins.colorizer,
		lazy = true,
		event = { 'BufRead' },
		config = function() require("config.colorizer") end,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		enabled = plugins.treesitter,
		lazy = true,
		build = ":TSUpdate",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-context",
			"nvim-treesitter/nvim-treesitter-textobjects",
			"nvim-treesitter/nvim-treesitter-refactor",
		},
		config = function() require("config.treesitter") end,
	},
	{
		"numToStr/Comment.nvim",
		enabled = plugins.comment,
		lazy = false,
		config = function() require("config.comment") end,
	},
	-- Autocomplete, linting and snippets
	{
		"neoclide/coc.nvim",
		enabled = plugins.coc,
		lazy = true,
		branch = 'release',
	},
	{
		"hrsh7th/nvim-cmp",
		enabled = plugins.nvimcmp,
		lazy = true,
		dependencies = {
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-omni",
			"f3fora/cmp-spell",
			"hrsh7th/cmp-emoji",
			"hrsh7th/cmp-cmdline",
			"petertriho/cmp-git",
			{ "hrsh7th/cmp-nvim-lsp",                enabled = plugins.lsp, },
			{ "onsails/lspkind-nvim",                enabled = plugins.lsp, },
			{ "quangnguyen30192/cmp-nvim-ultisnips", enabled = plugins.ultisnips, },
			{ "saadparwaiz1/cmp_luasnip",            enabled = plugins.luasnip, },
		},
		event = { 'InsertEnter', 'CmdlineEnter' },
		config = function() require("config.cmp") end,
	},
	{
		"neovim/nvim-lspconfig",
		enabled = plugins.lsp,
		lazy = false,
		dependencies = {
			"onsails/lspkind-nvim",
			{ "glepnir/lspsaga.nvim", enabled = plugins.lspsaga, config = function() require("config.lspsaga") end, },
			{
				"ray-x/lsp_signature.nvim",
				enabled = plugins.lspsignature,
				config = function()
					require(
						"config.lspsignature")
				end,
			},
		},
		config = function() require("config.lsp") end,
	},
	{
		"SirVer/ultisnips",
		enabled = plugins.ultisnips,
		lazy = true,
		dependencies = {
			{ "quangnguyen30192/cmp-nvim-ultisnips", enabled = plugins.cmp, },
			{ "fhill2/telescope-ultisnips.nvim",     enabled = plugins.telescope, },
		},
		event = 'InsertEnter',
		config = function() vim.cmd('source ~/.config/nvim/config/ultisnips.vim') end,
		-- Still measurely written using vimscript and python
		-- Allows python scripting
		-- Can you external software to port to vs-code
	},
	{
		"L3MON4D3/LuaSnip",
		enabled = plugins.luasnip,
		dependencies = {
			{ "saadparwaiz1/cmp_luasnip",         enabled = plugins.cmp, },
			{ "benfowler/telescope-luasnip.nvim", enabled = plugins.telescope, },
		},
		event = 'InsertEnter',
		config = function() require("config.luasnip") end,
		-- Written in lua - slightly better performance than ultisnips
		-- Allows lua scripting
		-- Has support for importing vs-code snippets
		-- Can keep a common database of snippets in vs-code format
		-- vs-code format is shit to begin with
	},
	-- Latex
	{
		"lervag/vimtex",
		enabled = plugins.tex,
		lazy = false,
		-- lazy = true,
		-- ft = { "tex" },
		config = function()
			vim.cmd("source ~/.config/nvim/core/globals.vim")
			vim.cmd("source ~/.config/nvim/config/vimtex.vim")
		end,
	},
	-- Nix
	{
		"LnL7/vim-nix",
		enabled = plugins.nix,
		lazy = true,
		ft = { 'nix' },
	},
	-- Utils
	{
		"godlygeek/tabular",
		enabled = plugins.tabular,
		lazy = false,
	},
	{
		"Konfekt/FastFold",
		enabled = plugins.fast_fold,
		lazy = false,
		-- Not needed since the folding issue was patched
		-- https://github.com/neovim/neovim/issues/5270
	},
	{
		'famiu/bufdelete.nvim',
		enabled = plugins.bufdelete,
		lazy = false,
	},
	{
		"ggandor/leap.nvim",
		enabled = plugins.leap,
		lazy = false,
		config = function() require("config.leap") end,
	},
	{
		"lewis6991/impatient.nvim",
		enabled = plugins.impatied,
		lazy = false,
		-- create complied code cache for plugins to allow fast loading
		-- remove after this gets merged into neovim
		-- https://github.com/neovim/neovim/pull/15436
	},
	{
		"akinsho/toggleterm.nvim",
		enabled = plugins.toggleterm,
		lazy = true,
		cmd = { "ToggleTerm" },
		setup = function() require("setup.toggleterm") end,
		config = function() require("config.toggleterm") end,
	},
	{
		"iamcco/markdown-preview.nvim",
		build = function()
			vim.fn["mkdp#util#install"]()
		end,
	},
	{
		"goolord/alpha-nvim",
		enabled = plugins.alpha,
		lazy = false,
		dependencies = "nvim-tree/nvim-web-devicons",
		config = function() require("config.alpha") end,
	}
}

return Spec
