-- Author: Anish Sevekari
-- Last Modified: Wed 26 Jan 2022 12:36:41 AM EST
-- # nvim-tree setup

vim.api.nvim_set_keymap('n', '<M-\\>', ':NvimTreeFocus<CR>', {
	noremap = true,
	silent = true
})

vim.api.nvim_set_keymap('n', '<leader>tr', ':NvimTreeRefresh<CR>', {
	noremap = true,
	silent = true
})

