-- Author: Anish Sevekari
-- Last Modified: Fri 01 Oct 2021 01:58:53 AM EDT
-- # treesitter settings

require('nvim-treesitter.configs').setup({
	ensure_installed = { 'python', 'cpp', 'haskell', 'lua', 'vim' },
	ignore_install = {},
	highlight = {
		enable = true,
		disable = {},
	},
})
