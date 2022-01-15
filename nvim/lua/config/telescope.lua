-- Author: Anish Sevekari
-- Last Modified: Sat Jan 15 16:53:31 2022
-- Telescope Plugin Config

local present, telescope = pcall(require,'telescope')

if not present then
	return
end

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
