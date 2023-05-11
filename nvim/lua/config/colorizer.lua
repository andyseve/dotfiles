-- Author: Anish Sevekari
-- Last Modified: Thu 17 Feb 2022 03:42:46 AM EST
-- # Colorizer Settings

local present, colorizer = pcall(require, "colorizer")
if not present then
	return
end

local config = {
	RGB      = true;         -- #RGB hex codes
	RRGGBB   = true;         -- #RRGGBB hex codes
	names    = false;        -- "Name" codes like Blue
	RRGGBBAA = false;        -- #RRGGBBAA hex codes
	rgb_fn   = true;         -- CSS rgb() and rgba() functions
	hsl_fn   = true;         -- CSS hsl() and hsla() functions
	css      = false;        -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
	css_fn   = false;        -- Enable all CSS *functions*: rgb_fn, hsl_fn
	-- Available modes: foreground, background
	mode     = 'background'; -- Set the display mode.
}

local filetypes = {
	css = { css = true; };
	html = { css = true; mode = 'foreground'; };
	bash = { names = true; };
	zsh = { names = true; };
	lua = { RGB = false; };
}


colorizer.setup(filetypes,config)
