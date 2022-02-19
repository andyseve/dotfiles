-- Author: Anish Sevekari
-- Last Modified: Fri 21 Jan 2022 12:04:34 PM EST
-- # indent-blankline settings

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

	filetype_exclude = {
		"help",
		"terminal",
		"dashboard",
		"packer",
		"lspinfo",
		"TelescopePrompt",
		"TelescopeResults",
		"nvchad_cheatsheet",
		"",
	},

	buftype_exclude = { "terminal" },
}
