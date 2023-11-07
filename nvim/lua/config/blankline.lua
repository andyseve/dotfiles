-- Author: Anish Sevekari
-- Last Modified: Fri 03 Jun 2022 10:40:14 AM EDT
-- # indent-blankline settings

local ts = require("core.user").plugins.treesitter
local present, blankline = pcall(require, 'ibl')

if not present then
	return
end

local config  = {
	enabled = true,
	debounce = 50,
	indent = {
		char = "",
		highlight = "IblIndent",
		smart_indent_cap = true,
		priority = 1,
	},
	whitespace = {
		highlight = "IblIndent",
		remove_blankline_trail = true,
	},
	scope = {
		enabled = true,
		char = "┊",
		show_start = true,
		show_end = false,
		injected_languages = true,
		highlight = "IblScope",
		priority = 50,
	},
	exclude = {
		filetypes = {
			"help",
			"terminal",
			"dashboard",
			"packer",
			"lspinfo",
			"TelescopePrompt",
			"TelescopeResults",
			"NvimTree",
			"alpha",
			"",
		},
		buftypes = {
			"terminal",
		},
	},
}

blankline.setup(config)
