-- Author: Anish Sevekari
-- Last Modified: Sat 04 Jun 2022 08:00:35 PM EDT
-- # nvim-tree setup

local utils = require("core.utils")

-- custom function to open nvim-tree
-- closes tree if you are already inside the tree window
-- focuses tree if tree window is open but you are not focused on it
-- opens tree if tree window is closed
local open_nvimtree = function()
	local ok, nvimtree_api = pcall(require, "nvim-tree.api")
	if not ok then
		return
	end
	if vim.bo.filetype == "NvimTree" then
		nvimtree_api.tree.close()
	else
		nvimtree_api.tree.focus()
	end
	vim.cmd("stopinsert")
end

local mappings = {}
mappings.nvim_tree = {
	whichkey = false,
	name = "פּ nvim_tree",
	n = {
		["<M-\\>"] = { open_nvimtree, "פּ Explorer" },
	},
	i = {
		["<M-\\>"] = { open_nvimtree, "פּ Explorer" },
	},
	v = {
		["<M-\\>"] = { open_nvimtree, "פּ Explorer" },
	}
}
mappings.windows = {
	name = " windows",
	prefix = "<leader>w",
	n = {
		["w"] = { open_nvimtree, "פּ Explorer" },
		["r"] = { "<cmd> NvimTreeRefresh <CR>", "פּ Refresh Explorer" },
	}
}

utils.load_mappings(mappings)
