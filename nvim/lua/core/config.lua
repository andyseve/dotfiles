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
	treesitter                  = true,
	comment                     = true,
	nerdcommenter               = false,

	-- themes
	solarized                   = true,
	onedark                     = true,

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
