-- Author: Anish Sevekari
-- Last Modified: Sat 04 Jun 2022 08:00:35 PM EDT
-- # nvim-tree setup
utils = require("core.utils")

vim.keymap.set("n", "<M-\\>", "<cmd> NvimTreeFocus <CR>", {noremap=true, silent=true})

local mappings = {}
mappings.nvim_tree = {
	n = {
		["<leader>tr"] = { "<cmd> NvimTreeRefresh <CR>", "Refresh Explorer" },
	},
}

utils.load_mappings(mappings)

