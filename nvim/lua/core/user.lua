-- Author: Anish Sevekari
-- Last Modified: Tue 07 Mar 2023 06:21:50 AM EST
-- Contains user config - Makes it more readable than the plugin configuration files
-- Provides basic variables which determine multiple settings

-- @plugin contains list of plugins to use
-- @theme contains starting theme

local user = {}
user.plugins = {
	-- ui
	bufferline = true,
	lualine    = true,
	notify     = true,
	noice      = true,
	blankline  = true,
	whichkey   = true,
	zenmode    = true,
	gitsigns   = true,
	trouble    = true,
	twilight   = true,

	-- themes
	catppuccin = true,
	solarized  = true,
	onedark    = true,
	base16     = false,

	-- search and files
	telescope = true,
	nvimtree  = true,
	undotree  = nil, -- use telescope plugin instead
	diffview  = true,

	-- syntax
	autopairs  = true,
	matchup    = true,
	surround   = true,
	colorizer  = true,
	treesitter = true,
	comment    = true,

	-- autocomplete and linting
	coc          = false,
	nvimcmp      = true,
	lsp          = true,
	lspsaga      = true,
	lspsignature = true,
	ultisnips    = true,
	luasnip      = false,


	-- filetype specifics
	tex = true,
	nix = true,

	-- utils
	tabular    = true,
	fast_fold  = false,
	bufdelete  = true,
	leap       = true,
	impatient  = true,
	toggleterm = true,
	alpha      = true,
}


if user.plugins.undotree == nil then
	user.plugins.undotree = not user.plugins.telescope
end

user.theme = "catppuccin"
user.use_catppuccin = true

return user
