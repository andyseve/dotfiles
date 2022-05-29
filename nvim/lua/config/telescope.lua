-- Author: Anish Sevekari
-- Last Modified: Fri 27 May 2022 07:58:08 AM EDT
-- Telescope Plugin Config

local present, telescope = pcall(require,'telescope')

if not present then
	return
end

telescope.setup {
	defaults = {
		layout_strategy = "flex",
		layout_config = {
			horizontal = {
				width = 0.8,
				height = 0.9,
				mirror = false,
				preview_cutoff = 80,
				preview_width = 0.6,
			},
			vertical = {
				width = 0.5,
				height = 0.9,
				prompt_position = "top",
				mirror = false,
				preview_cutoff = 20,
				preview_height = 0.5,
			},
			flex = {
				flip_columns = 200,
				flip_lines = 20,
			},
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
		ag = {},
		file_browser = {
			theme = "ivy",
			hijack_netrw = true, -- disables netrw and use telescope-file-browser in its place
		},
		ultisnips = {},
		packer = {
			theme = "ivy",
			layout_config = {
				height = 0.5,
			},
		},
	},
}

pcall(telescope.load_extension, 'fzf-native')
pcall(telescope.load_extension, 'ultisnips')
pcall(telescope.load_extension, 'file_browser')
pcall(telescope.load_extension, 'packer')
pcall(telescope.load_extension, 'ag')
