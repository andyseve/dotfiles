local utils = require("core.utils")
-- read wal colors
local home = os.getenv("HOME")
local wal_cache_dir = home .. "/.cache/wal"
local wal_cache_file = wal_cache_dir .. "/colors"
local wal_colors_file = io.open(wal_cache_file, "rb")
if not wal_colors_file then 
	utils.inspect("wal cache file not found at " .. wal_cache_file)
	return nil 
end
local wal_colors = {}
while true do
	local line = wal_colors_file:read "*line"
	if line == nil then break end
	table.insert(wal_colors, line)
end
wal_colors_file:close()

-- load hsluv utils
local hsluv_present, hsluv = pcall(require, "catppuccin.lib.hsluv")
if not hsluv_present then 
	-- use linear rgb transformations instead?
	return nil
end

local generate_mocha = function(colors)
	local hex_base = colors[1]
	local hsl_base = hsluv.hex_to_hsluv(hex_base)
	utils.inspect(hsl_base)
	local hex_text = colors[16]
	local hsl_text = hsluv.hex_to_hsluv(hex_text)
	utils.inspect(hsl_text)
	local hex_overlay = colors[9]
	local hsl_overlay = hsluv.hex_to_hsluv(hex_overlay)
	utils.inspect(hsl_overlay)
	return {
		rosewater = "#CB4661", -- 50, 55, 15
		flamingo  = "#C42D45", -- 45, 60, 25
		pink      = colors[6], -- 45, 65,-05
		mauve     = "#A95F89", -- 45, 35,-10
		red       = colors[2], -- 50, 65, 45
		maroon    = "#CB4B16", -- 50, 50, 55
		peach     = "#B95C00", -- 50, 35, 60
		yellow    = colors[4], -- 60, 10, 65
		green     = colors[3], -- 60,-20, 65
		teal      = colors[7], -- 60,-35,-05
		sky       = "#12948B", -- 55,-35,-05
		sapphire  = "#00877E", -- 50,-35,-05
		blue      = colors[5], -- 55,-10,-45
		lavender  = "#7092CE", -- 60,-00,-35

		text      = colors[16], -- 60,-06,-03
		subtext1  = "#768789", -- 55,-06,-03
		subtext0  = "#6A7A7C", -- 50,-06,-03
		overlay2  = "#4C6269", -- 40,-07,-07
		overlay1  = colors[9], -- 35,-07,-07
		overlay0  = "#354A51", -- 30,-07,-07
		surface2  = "#234D59", -- 30,-12,-12
		surface1  = "#16414D", -- 25,-12,-12
		surface0  = "#073642", -- 20,-12,-12

		base      = colors[1], -- 15,-12,-12
		mantle    = "#00202B", -- 10,-12,-12
		crust     = "#001721", -- 05,-12,-12
	}
end

local theme = {}

theme.colors = {
	mocha = generate_mocha(wal_colors),
}

theme.custom_highlights = function(colors)
	return {
		FloatBorder = { bg = colors.crust, fg = colors.lavender },
		VertSplit = { bg = colors.base, fg = colors.surface0 },
		CursorLineNr = { fg = colors.lavender, style = { "bold" } },
		NvimTreeIndentMarker = { link = "IndentBlanklineChar" },
	}
end

return theme
