-- Mapping for nvim
-- Author: Anish Sevekari
-- Last Modified: Mon 06 Mar 2023 11:09:57 PM EST

local plugins = require("core.user").plugins
local utils = require("core.utils")
local present, telescope = pcall(require, "telescope")
if not present then
	return
end

local builtin = require("telescope.builtin")

local telescope_ag = require("telescope-ag")
telescope_ag.setup()


local config = {
	defaults = {
		previewer = true,
	},
	extensions = {
		fzf = {
			fuzzy = true,
			override_generic_sorter = true,
			override_file_sorter = true,
			case_mode = "smart_case",
		},
		file_browser = {
			-- disables netrw and use telescope-file-browser in its place
			hijack_netrw = true,
			mappings = {
				["i"] = {
					-- your custom insert mode mappings
				},
				["n"] = {
					-- your custom normal mode mappings
				},
			},
		},
		undo = {
			use_delta = true,
			use_custom_command = nil, -- setting this implies `use_delta = false`. Accepted format is: { "bash", "-c", "echo '$DIFF' | delta" }
			side_by_side = false,
			diff_context_lines = vim.o.scrolloff,
			entry_format = "state #$ID, $STAT, $TIME",
			mappings = {
				i = {
					-- IMPORTANT: Note that telescope-undo must be available when telescope is configured if
					-- you want to replicate these defaults and use the following actions. This means
					-- installing as a dependency of telescope in it's `requirements` and loading this
					-- extension from there instead of having the separate plugin definition as outlined
					-- above.
					["<cr>"] = require("telescope-undo.actions").yank_additions,
					["<S-cr>"] = require("telescope-undo.actions").yank_deletions,
					["<C-cr>"] = require("telescope-undo.actions").restore,
				},
				n = {
					["y"] = require("telescope-undo.actions").yank_additions,
					["Y"] = require("telescope-undo.actions").yank_deletions,
					["u"] = require("telescope-undo.actions").restore,
				},
			},
		},
		lazy = {
			-- Optional theme (the extension doesn't set a default theme)
			theme = "ivy",
			-- Whether or not to show the icon in the first column
			show_icon = true,
			-- Mappings for the actions
			mappings = {
				open_in_browser = "<C-o>",
				open_in_file_browser = "<M-b>",
				open_in_find_files = "<C-f>",
				open_in_live_grep = "<C-g>",
				open_plugins_picker = "<C-b>", -- Works only after having called first another action
				open_lazy_root_find_files = "<C-r>f",
				open_lazy_root_live_grep = "<C-r>g",
			},
			-- Other telescope configuration options
		},
		glyph = {
			action = function(glyph)
				-- Get row and column cursor,
				-- use unpack because it's a tuple.
				local row, col = unpack(vim.api.nvim_win_get_cursor(0))
				vim.api.nvim_buf_set_text(0, row - 1, col, row - 1, col, { glyph.value })
			end
		},
	},
}

telescope.setup(config)
telescope.load_extension("mappings")
telescope.load_extension("file_browser")
telescope.load_extension("fzf")
telescope.load_extension("lazy")
telescope.load_extension("glyph") -- I need this for modding my vim
if plugins.notify then telescope.load_extension("notify") end
if utils.is_exe("ag") then telescope.load_extension("ag") end
if not plugins.undotree then telescope.load_extension("undo") end
if plugins.ultisnips then telescope.load_extension("ultisnips") end
if plugins.luasnip then telescope.load_extension("luasnip") end
