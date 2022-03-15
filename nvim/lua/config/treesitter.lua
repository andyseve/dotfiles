-- Author: Anish Sevekari
-- Last Modified: Tue 15 Mar 2022 02:50:52 AM EDT
-- # treesitter settings

local present, ts_configs = pcall(require, 'nvim-treesitter.configs')

if not present then
	return
end

ts_configs.setup({
	ensure_installed = {
		'python',
		'cpp',
		'haskell',
		'lua',
		'vim'
	},

	ignore_install = {},
	highlight = {
		enable = true,
		disable = { 'latex' },
		use_languagetree = true
	},

})
