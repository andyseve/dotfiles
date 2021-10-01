-- Author: Anish Sevekari
-- Last Modified: Thu 30 Sep 2021 11:43:18 PM EDT
-- # zen-mode settings

require("zen-mode").setup({
	window = {
		backdrop = 0.8,
	},
})

vim.cmd([[
	augroup auto_zen
		autocmd!
		autocmd FileType Markdown ZenMode
	augroup END
]])
