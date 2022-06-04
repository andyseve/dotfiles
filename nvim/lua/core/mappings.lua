-- Mapping for nvim
-- Author: Anish Sevekari
-- Last Modified: Sat 04 Jun 2022 02:58:02 AM EDT

-- n, v, i are mode names
-- supports whichkey through load_mappings defined in utils

local utils = require("core.utils")

local mappings = {}

mappings.general = {
	i = {
		-- navigate within insert mode
		["<C-h>"] = { "<Left>", " move left" },
		["<C-l>"] = { "<Right>", " move right" },
		["<C-j>"] = { "<Down>", " move down" },
		["<C-k>"] = { "<Up>", " move up" },
	},
	n = {

		["<ESC>"] = { "<cmd> noh <CR>", " no highlight" },

		-- switch between windows
		["<C-h>"] = { "<C-w>h", " window left" },
		["<C-l>"] = { "<C-w>l", " window right" },
		["<C-j>"] = { "<C-w>j", " window down" },
		["<C-k>"] = { "<C-w>k", " window up" },

		-- save
		["<C-s>"] = { "<cmd> w <CR>", "﬚ save file" },

		-- Copy all
		["<C-c>"] = { "<cmd> %y+ <CR>", " copy whole file" },
	}
}

utils.load_mappings(mappings)

return mappings
