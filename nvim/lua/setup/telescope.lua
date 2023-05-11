-- Author: Anish Sevekari
-- Last Modified: Sat 04 Jun 2022 08:10:55 PM EDT
-- # telescope setup
local utils = require("core.utils")
local plugins = require("core.user").plugins

local mappings = {}
mappings.files = {
	name = " files",
	prefix = "<leader>f",
	n = {
		["f"] = { "<cmd> Telescope find_files <CR>", " Find files" },
		["r"] = { "<cmd> Telescope oldfiles <CR>", " Recent files" },
		["g"] = { "<cmd> Telescope live_grep <CR>", " Grep files" },
		["b"] = { "<cmd> Telescope buffers <CR>", "﬘ Find buffers" },
		["h"] = { "<cmd> Telescope help_tags <CR>", " Help tags" },
	},
}
mappings.lists = {
	name = " lists",
	prefix = "<leader>o",
	n = {
		["q"] = { "<cmd> Telescope quickfix <CR>", " Quickfix" },
		["w"] = { "<cmd> Telescope loclist <CR>", " Locations" },
		["g"] = { "<cmd> Telescope glyph <CR>", " Glyph" }
	}
}
if plugins.ultisnip then
	mappings.lists.n["s"] = { "<cmd> Telescope ultisnips theme=ivy <CR>", "Snippets" }
end
mappings.diagnostics = {
	name = " diagnostics",
	prefix = "<leader>d",
	n = {
		["i"] = { "<cmd> Telescope diagnostics <CR>", " Diagnostics" },
		["m"] = { "<cmd> Telescope notify <CR>", " Messages" },
	}
}
if plugins.noice then
	mappings.diagnostics.n["n"] = { "<cmd> Noice telescope <CR>", " Messages" }
end
mappings.edit = {
	name = " edit",
	prefix = "<leader>e",
	n = {
		["e"] = { "<cmd> Telescope find_files dir=~/dotfiles <CR>", "ﱐ Find files" },
	}
}

utils.load_mappings(mappings)
