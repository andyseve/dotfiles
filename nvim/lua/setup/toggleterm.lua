-- Author: Anish Sevekari
-- Last Modified: Sat 04 Jun 2022 08:00:35 PM EDT
-- # toggleterm setup

local utils = require("core.utils")

local mappings = {}
mappings.toggleterm = {
	whichkey = false,
	name = " terminal",
	n = {
		["<C-`>"] = { "<cmd> ToggleTerm <CR>", " Terminal" },
	},
	i = {
		["<C-`>"] = { "<cmd> ToggleTerm <CR>", " Terminal" },
	},
	v = {
		["<C-`>"] = { "<cmd> ToggleTerm <CR>", " Terminal" },
	}
}

utils.load_mappings(mappings)
