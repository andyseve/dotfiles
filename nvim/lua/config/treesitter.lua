-- Author: Anish Sevekari
-- Last Modified: Fri 28 Oct 2022 06:04:46 AM EDT
-- # treesitter settings

local present, ts_configs = pcall(require, 'nvim-treesitter.configs')

if not present then
	return
end

ts_configs.setup({
	ensure_installed = {
		'python',
		'cpp',
		'haskell',
		'lua',
		'vim'
	},
	-- ignore_install = {}, -- this is only applicable if ensure_installed = all

	sync_install = false,
	auto_install = false,

	highlight = {
		enable = true,
		disable = { 'latex' },
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
