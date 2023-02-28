-- Author: Anish Sevekari
-- Last Modified: Thu 23 Feb 2023 11:19:04 PM EST
-- # treesitter settings

local present, ts_configs = pcall(require, 'nvim-treesitter.configs')

if not present then
	return
end

ts_configs.setup({
	ensure_installed = {
		'c',
		'cpp',
		'python',
		'haskell',
		'lua',
		'vim',
		'nix',
		'bash',
		'bibtex',
		'make'
	},
	-- ignore_install = {}, -- this is only applicable if ensure_installed = all

	sync_install = true,
	auto_install = false,

	highlight = {
		enable = true,
		disable = { 'latex' }, -- treesitter conflicts vimtex
		additional_vim_regex_highlighting = false,
	},

	indent = {
		-- Experimental
		-- Enables indent = symbol based on treesitter
		enable = true,
	},

	matchup = {
		enable = true;
	},

	incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },

})
