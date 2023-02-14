-- Author: Anish Sevekari
-- Last Modified: Sat 04 Jun 2022 08:10:55 PM EDT
-- # telescope setup
utils = require("core.utils")

local mappings = {}
local groups = {}
mappings.files = {
	n = {
		["<leader>ff"] = { "<cmd> Telescope find_files <CR>", "Find files" },
		["<leader>fr"] = { "<cmd> Telescope oldfiles <CR>", "Recent files" },
		["<leader>fg"] = { "<cmd> Telescope live_grep <CR>", "Grep files" },
		["<leader>fb"] = { "<cmd> Telescope buffers <CR>", "Find buffers" },
		["<leader>fh"] = { "<cmd> Telescope help_tags <CR>", "Help tags" },
		["<c-x><c-s>"] = { "<cmd> Telescope ultisnips theme=ivy <CR>", "Snippets" },
	},
}

groups["files"] = "<leader>f"

utils.load_mappings(mappings)
utils.add_mapping_groups(groups)
