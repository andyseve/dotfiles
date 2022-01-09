-- Author: Anish Sevekari
-- Last Modified: Sat Jan  8 15:40:01 2022
-- # treesitter settings

require('nvim-treesitter.configs').setup({
	ensure_installed = { 'python', 'cpp', 'haskell', 'lua', 'vim' },
	ignore_install = {},
	highlight = {
		enable = true,
		disable = {},
	},
})
