-- Author: Anish Sevekari
-- Last Modified: Sat Jan 15 16:34:53 2022
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
		disable = {},
		use_languagetree = true
	},

})
