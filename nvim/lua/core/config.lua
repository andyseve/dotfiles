-- Author: Anish Sevekari
-- Last Modified: Sat 04 Jun 2022 03:05:37 AM EDT
-- Contains plugin config - Makes it more readable than changing packer file.
local config= {}

config.plugins = {
	-- ui
	bufferline                  = true,
	feline                      = true,
	notify                      = false,
	blankline                   = true,
	which_key                   = true,
	bufdelete                   = true,

	-- autocomplete and linting
	coc                         = false,
	lsp_config                  = true,
	nvim_cmp                    = true,
	ultisnips                   = true,
	luasnip                     = false,

	-- syntax
	colorizer                   = true,
	treesitter                  = true,
	comment                     = true,

	-- themes
	solarized                   = true,
	onedark                     = true,
	nvim_base16                 = true,

	-- search and files
	telescope                   = true,
	nvim_tree                   = true,
	mundo                       = true,
	gitsigns                    = true,

	-- pairs
	nvim_autopairs              = true,
	vim_matchup                 = true,
	vim_surround                = true,

	-- filetype specifics
	tex                         = true,
	nix                         = true,

	-- others
	true_zen                    = true,
	tabular                     = true,
	fast_fold                   = true
}

return config
