local theme = {}

theme.colors = {
	latte = {
		rosewater = "#AD2849", -- 40, 55, 15
		flamingo  = "#A6002F", -- 35, 60, 25
		pink      = "#A60058", -- 35, 65,-05
		mauve     = "#803963", -- 35, 35,-10
		red       = "#BD001A", -- 40, 65, 45
		maroon    = "#AC3000", -- 40, 50, 55
		peach     = "#9B4300", -- 40, 35, 60
		yellow    = "#987000", -- 50, 10, 65
		green     = "#6A7F00", -- 50,-20, 65
		teal      = "#00877E", -- 50,-35,-05
		sky       = "#007A72", -- 45,-35,-05
		sapphire  = "#006D65", -- 40,-35,-05
		blue      = "#0072B6", -- 45,-10,-45
		lavender  = "#567983", -- 50,-00,-35

		text     = "#657B83", -- 50,-07,-07
		subtext1 = "#71888F", -- 55,-07,-07
		subtext0 = "#7E959D", -- 60,-07,-07
		overlay2 = "#86948F", -- 70,-06, 01
		overlay1 = "#93A19C", -- 75,-06, 01
		overlay0 = "#A0AeA9", -- 80,-06, 01
		surface2 = "#B1AB99", -- 70, 00, 00
		surface1 = "#BEB8A6", -- 75, 00, 10
		surface0 = "#CCC6B4", -- 80, 00, 10

		base   = "#F7F0DD", -- 95, 00, 10
		mantle = "#E9E2CF", -- 90, 00, 10
		crust  = "#DAD4C1", -- 85, 00, 00
	},
	frappe = {
		rosewater = "#EA627A", -- 60, 55, 15
		flamingo  = "#E34C5D", -- 55, 60, 25
		pink      = "#E2468F", -- 55, 65,-05
		mauve     = "#B66B96", -- 55, 35,-10
		red       = "#FF6F5E", -- 70, 65, 45
		maroon    = "#FF8248", -- 70, 50, 55
		peach     = "#D7761F", -- 60, 35, 60
		yellow    = "#D2A328", -- 70, 10, 65
		green     = "#A1B420", -- 70,-20, 65
		teal      = "#4DBDB3", -- 70,-35,-05
		sky       = "#3CAFA5", -- 65,-35,-05
		sapphire  = "#2AA198", -- 60,-35,-05
		blue      = "#3DA6EE", -- 65,-10,-45
		lavender  = "#8CADEB", -- 70,-00,-35

		text     = "#9DAFB0", -- 70,-06,-03
		subtext1 = "#90A1A3", -- 65,-06,-03
		subtext0 = "#839496", -- 60,-06,-03
		overlay2 = "#657B83", -- 50,-07,-07
		overlay1 = "#586E76", -- 45,-07,-07
		overlay0 = "#4C6269", -- 40,-07,-07
		surface2 = "#234D59", -- 30,-12,-12
		surface1 = "#16414D", -- 25,-12,-12
		surface0 = "#073642", -- 20,-12,-12

		base   = "#002B36", -- 15,-12,-12
		mantle = "#00202B", -- 10,-12,-12
		crust  = "#001721", -- 05,-12,-12
	},
	macchiato = {
		rosewater = "#AD2849", -- 40, 55, 15
		flamingo  = "#A6002F", -- 35, 60, 25
		pink      = "#A60058", -- 35, 65,-05
		mauve     = "#803963", -- 35, 35,-10
		red       = "#BD001A", -- 40, 65, 45
		maroon    = "#AC3000", -- 40, 50, 55
		peach     = "#9B4300", -- 40, 35, 60
		yellow    = "#987000", -- 50, 10, 65
		green     = "#6A7F00", -- 50,-20, 65
		teal      = "#00877E", -- 50,-35,-05
		sky       = "#007A72", -- 45,-35,-05
		sapphire  = "#006D65", -- 40,-35,-05
		blue      = "#0072B6", -- 45,-10,-45
		lavender  = "#567983", -- 50,-00,-35

		text     = "#6A7A7C", -- 50,-06,-03
		subtext1 = "#566E6F", -- 45,-06,-03
		subtext0 = "#526163", -- 40,-06,-03
		overlay2 = "#4C6269", -- 40,-07,-07
		overlay1 = "#41565D", -- 35,-07,-07
		overlay0 = "#354A51", -- 30,-07,-07
		surface2 = "#133F4B", -- 24,-12,-12
		surface1 = "#0A3844", -- 21,-12,-12
		surface0 = "#01313D", -- 18,-12,-12

		base   = "#002B36", -- 15,-12,-12
		mantle = "#002430", -- 12,-12,-12
		crust  = "#001E29", -- 09,-12,-12
	},
	mocha = {
		rosewater = "#CB4661", -- 50, 55, 15
		flamingo  = "#C42D45", -- 45, 60, 25
		pink      = "#C42376", -- 45, 65,-05
		mauve     = "#A95F89", -- 45, 35,-10
		red       = "#DC322F", -- 50, 65, 45
		maroon    = "#CB4B16", -- 50, 50, 55
		peach     = "#B95C00", -- 50, 35, 60
		yellow    = "#B58900", -- 60, 10, 65
		green     = "#859900", -- 60,-20, 65
		teal      = "#2AA198", -- 60,-35,-05
		sky       = "#12948B", -- 55,-35,-05
		sapphire  = "#00877E", -- 50,-35,-05
		blue      = "#268BD2", -- 55,-10,-45
		lavender  = "#7092CE", -- 60,-00,-35

		text     = "#839496", -- 60,-06,-03
		subtext1 = "#768789", -- 55,-06,-03
		subtext0 = "#6A7A7C", -- 50,-06,-03
		overlay2 = "#4C6269", -- 40,-07,-07
		overlay1 = "#41565D", -- 35,-07,-07
		overlay0 = "#354A51", -- 30,-07,-07
		surface2 = "#234D59", -- 30,-12,-12
		surface1 = "#16414D", -- 25,-12,-12
		surface0 = "#073642", -- 20,-12,-12

		base   = "#002B36", -- 15,-12,-12
		mantle = "#00202B", -- 10,-12,-12
		crust  = "#001721", -- 05,-12,-12
	},
}

theme.custom_highlights = function(colors)
	return {
		FloatBorder = { bg = colors.crust, fg = colors.lavender },
		VertSplit = { bg = colors.base, fg = colors.surface0 },
		CursorLine = { bg = colors.base },
		CursorLineNr = { fg = colors.lavender, style = { "bold" } },
		NvimTreeIndentMarker = { link = "IndentBlanklineChar" },
		Folded = { fg = colors.sapphire, bg = O.transparent_background and colors.none or colors.mantle, style = { "italic" } }
	}
end

return theme
