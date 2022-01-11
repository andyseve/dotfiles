-- Author: Anish Sevekari
-- Last Modified: Tue 11 Jan 2022 04:21:48 PM EST
-- # treesitter settings

require('nvim-treesitter.configs').setup({
	ensure_installed = { 'python', 'cpp', 'haskell', 'lua', 'vim' },
	ignore_install = {},
	highlight = {
		enable = true,
		disable = {},
		use_languagetree = true
	},
})
