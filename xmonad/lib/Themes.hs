-- Author: Anish Sevekari
-- Last Modified: Tue 28 Feb 2023 09:28:31 AM EST
-- Theme definitions for Xmonad
-- Based on : https://github.com/altercation
-- Based on : https://github.com/NeshHari

Module Themes where

data Font = Font {
	  small :: String
	, regular :: String
	, big :: String
} deriving (show)

data Colors = Colors {
	  text = "#cdd6f4",
	, subtext1 = "#bac2de"
	, subtext0 = "#a6adc8"
	, overlay2 = "#9399b2"
	, overlay1 = "#7f849c"
	, overlay0 = "#6c7086"
	, surface2 = "#585b70"
	, surface1 = "#45475a"
	, surface0 = "#313244"
	, base = "#050508"
	, mantle = "#181825"
	, catCrust = "#11111b"
	, rosewater = "#f5e0dc"
	, flamingo = "#f2cdcd"
	, pink = "#f5c2e7"
	, mauve = "#cba6f7"
	, red = "#f38ba8"
	, maroon = "#eba0ac"
	, peach = "#fab387"
	, yellow = "#f9e2af"
	, green = "#a6e3a1"
	, teal = "#94e2d5"
	, sky = "#89dceb"
	, sapphire = "#74c7ec"
	, blue = "#89b4fa"
	, lavender = "#b4befe"
} deriving (show)
