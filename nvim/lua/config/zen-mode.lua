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
