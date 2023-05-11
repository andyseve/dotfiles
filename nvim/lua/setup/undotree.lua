local utils = require("core.utils")

local mappings = {}
mappings.undo = {
	whichkey = false,
	n = {
		["<leader>u"] = { "<CMD>UndotreeToggle<CR>", "Undo" },
	}
}

utils.load_mappings(mappings)
