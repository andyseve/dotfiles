-- Author: Anish Sevekari
-- Last Modified: Fri Jan 21 11:04:45 2022
-- Contains plugin config - Makes it more readable than changing packer file.
local config= {}

config.plugins = {
	-- ui
	bufferline                  = true,
	feline                      = true,
	notify                      = false,
	blankline                    = true,

	-- autocomplete and linting
	coc                         = false,
	lsp_config                  = true,
	nvim_cmp                    = true,
	ultisnips                   = true,
	luasnip                     = false,

	-- syntax
	colorizer                   = false,
	treesitter                  = false,
	comment                     = true,
	nerdcommenter               = false,

	-- themes
	solarized                   = true,
	onedark                     = false,

	-- search and files
	telescope                   = true,
	fzf                         = false,
	nvim_tree                   = true,
	nerdtree                    = false,
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
	indent_blankline            = true,
	fast_fold                   = true
}

return config
