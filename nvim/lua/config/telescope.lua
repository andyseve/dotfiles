-- Author: Anish Sevekari
-- Last Modified: Wed 12 Jan 2022 09:20:51 AM EST
-- Telescope Plugin Config

local telescope = require('telescope')

telescope.setup {
	defaults = {
		layout = "horizontal",
		layout_strategy = {
			horizontal = {
				prompt_position = "top",
				height = 0.9,
				width = 0.8,
			},
			verticle = {
				mirror = "false",
				height = 0.9,
				width = 0.5
			},
			preview_cutoff = 120
		}
	}
}
