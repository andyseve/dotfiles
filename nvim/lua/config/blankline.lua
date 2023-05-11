-- Author: Anish Sevekari
-- Last Modified: Fri 03 Jun 2022 10:40:14 AM EDT
-- # indent-blankline settings

local ts = require("core.user").plugins.treesitter
local present, blankline = pcall(require, 'indent_blankline')

if not present then
	return
end

blankline.setup {
	char = "",
	space_char_blankline = " ",
	show_current_context = true,
	show_current_context_start = true,
	show_trailing_blankline_indent = false,
	show_first_indent_level = false,
	use_treesitter = ts,
	use_treesitter_scope = ts,

	filetype_exclude = {
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

	buftype_exclude = { "terminal" },
}
