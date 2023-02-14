-- Author: Anish Sevekari
-- Last Modified: Fri Oct 28 13:20:52 2022
-- Contains plugin config - Makes it more readable than changing packer file.
local config= {}

config.plugins = {
	-- ui
	bufferline                  = true,
	feline                      = true,
	notify                      = true,
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
	fast_fold                   = true,
}

return config
