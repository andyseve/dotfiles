-- Author: Anish Sevekari
-- Last Modified: Sat 04 Jun 2022 08:10:55 PM EDT
-- # trouble setup
local utils = require("core.utils")

local mappings = {}
mappings.trouble = {
	name = " Trouble",
	prefix = "<leader>x",
	n = {
		["x"] = { function() require("trouble").toggle() end, "Toggle" },
		["s"] = { function() require("trouble").toggle("workspace_diagnostics") end, "Workspace" },
		["d"] = { function() require("trouble").toggle("document_diagnostics") end, "Document" },
		["q"] = { function() require("trouble").toggle("quickfix") end, " Quickfix" },
		["w"] = { function() require("trouble").toggle("loclist") end, " Loclist" },
	},
}

utils.load_mappings(mappings)
