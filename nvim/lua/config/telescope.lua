-- Author: Anish Sevekari
-- Last Modified: Fri 27 May 2022 06:45:09 AM EDT
-- Telescope Plugin Config

local present, telescope = pcall(require,'telescope')

if not present then
	return
end

telescope.setup {
	defaults = {
		layout_strategy = "horizontal",
		layout_config = {
			horizontal = {
				prompt_position = "top",
				height = 0.9,
				width = 0.8,
				preview_width = 0.55,
				results_width = 0.8,
			},
			vertical = {
				mirror = "false",
				height = 0.9,
				width = 0.5
			},
			preview_cutoff = 120
		},
		initial_mode = "insert",
		selection_strategy = "reset",
		sorting_strategy = "ascending",
		file_sorter = require('telescope.sorters').get_fuzzy_file,
		file_ignore_patterns = { "node_modules" },
		generic_sorter = require('telescope.sorters').get_generic_fuzzy_sorter,
		path_display = { "truncate" },
		color_devicons = true,
		use_less = true,
		file_previewer = require('telescope.previewers').vim_buffer_cat.new,
		grep_previewer = require('telescope.previewers').vim_buffer_vimgrep.new,
		qflist_previewer = require('telescope.previewers').vim_buffer_qflist.new,
		mappings = {
			n = {
				["q"] = require('telescope.actions').close,
				["h"] = "which_key",
			},
			i = {
				["<c-q>"] = require('telescope.actions').close,
				["<c-h>"] = "which_key",
			},
		},
	},
	pickers = {

	},
	extensions = {
		fzf = {
			fuzzy = true,                   -- false will only do exact matching
			override_generic_sorter = true, -- override the generic sorter
			override_file_sorter = true,    -- override the file sorter
			case_mode = "smart_case",
		},
	},
}

pcall(telescope.load_extension, 'fzf')
