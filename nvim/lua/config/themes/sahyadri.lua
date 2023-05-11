local theme = {}

theme.colors = {
	latte = {
		rosewater = "#c14a4a",
		flamingo  = "#c14a4a",
		pink      = "#945e80",
		mauve     = "#945e80",
		red       = "#c14a4a",
		maroon    = "#c14a4a",
		peach     = "#c35e0a",
		yellow    = "#a96b2c",
		green     = "#6c782e",
		teal      = "#4c7a5d",
		sky       = "#4c7a5d",
		sapphire  = "#4c7a5d",
		blue      = "#45707a",
		lavender  = "#45707a",

		text     = "#654735",
		subtext1 = "#7b5d44",
		subtext0 = "#8f6f56",
		overlay2 = "#a28368",
		overlay1 = "#b6977a",
		overlay0 = "#c9aa8c",
		surface2 = "#A79C86",
		surface1 = "#C9C19F",
		surface0 = "#DFD6B1",

		base   = "#fbf1c7",
		mantle = "#F3EAC1",
		crust  = "#E7DEB7",
	},
	mocha = {
		rosewater = "#EA6962",
		flamingo  = "#EA6962",
		pink      = "#D3869B",
		mauve     = "#D3869B",
		red       = "#EA6962",
		maroon    = "#EA6962",
		peach     = "#BD6F3E",
		yellow    = "#D8A657",
		green     = "#A9B665",
		teal      = "#89B482",
		sky       = "#89B482",
		sapphire  = "#89B482",
		blue      = "#6889ff", --60,100,260
		lavender  = "#449da4", --60,080,200

		text     = "#7f9777", --60,030,120
		subtext1 = "#687d62", --50,030,120
		subtext0 = "#52634d", --40,030,120
		overlay2 = "#545346", --35,025,080
		overlay1 = "#49473c", --30,025,080
		overlay0 = "#3d3c32", --25,070,080
		surface2 = "#392e30", --20,010,000
		surface1 = "#2f2527", --16,010,000
		surface0 = "#251e1f", --12,010,000

		base   = "#221920", -- 10,020,320
		mantle = "#1b161a", -- 08,010,320
		crust  = "#151314", -- 06,005,320
	},
}

theme.custom_highlights = function(colors)
	return {
		NormalFloat = { fg = C.text, bg = (O.transparent_background and vim.o.winblend == 0) and C.none or C.crust },
		VertSplit = { bg = colors.base, fg = colors.surface0 },
		CursorLineNr = { fg = colors.lavender, style = { "bold" } },
		Pmenu = { bg = colors.crust, fg = "" },
		PmenuSel = { bg = colors.surface0, fg = "" },
		TelescopeSelection = { fg = colors.green, bg = colors.surface0 },
		TelescopePromptCounter = { fg = colors.mauve, style = { "bold" } },
		TelescopePromptPrefix = { bg = colors.surface0 },
		TelescopePromptNormal = { bg = colors.surface0 },
		TelescopeResultsNormal = { bg = colors.mantle },
		TelescopePreviewNormal = { bg = colors.crust },
		TelescopePromptBorder = { bg = colors.surface0, fg = colors.surface0 },
		TelescopeResultsBorder = { bg = colors.mantle, fg = colors.mantle },
		TelescopePreviewBorder = { bg = colors.crust, fg = colors.crust },
		TelescopePromptTitle = { fg = colors.green, bg = colors.surface0 },
		TelescopeResultsTitle = { fg = colors.green, bg = colors.mantle },
		TelescopePreviewTitle = { fg = colors.green, bg = colors.crust },
		NvimTreeIndentMarker = { link = "IndentBlanklineChar" },
	}
end

return theme
