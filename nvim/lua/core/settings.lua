-- Neovim core settings
-- Author: Anish Sevekari
-- Last Modified: Wed 08 Mar 2023 01:33:34 AM EST

local opt = vim.opt
local utils = require("core.utils")

opt.number = true
opt.relativenumber = true

opt.autoindent = true
opt.smartindent = true
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = false

opt.breakindent = true
opt.showbreak = "↳ "

opt.ignorecase = true
opt.smartcase = true

opt.hlsearch = true
opt.incsearch = true

opt.termguicolors = true

opt.scrolloff = 8
opt.signcolumn = "yes"
-- opt.signcolumn = "no"
opt.cursorline = true

opt.updatetime = 50

opt.mouse = "a"
opt.swapfile = false

opt.completeopt = "menu,menuone,noselect,noinsert"
opt.clipboard = "unnamedplus"


opt.timeoutlen = 1000
opt.ttimeoutlen = 1000

opt.listchars:append("trail:⋅")
opt.listchars:append("extends:↝")
opt.listchars:append("precedes:↜")

opt.spelllang = "en_us"

-- fold settings
function _G.foldtext_right_align()
	local line_count = vim.v.foldend - vim.v.foldstart + 1
	local line_count_string = "↝" .. line_count

	local line = vim.fn.getline(vim.v.foldstart)
	line = string.gsub(line, '{{{', '')
	local commentstring = vim.bo.commentstring
	for c in string.gmatch(commentstring, "(%s)+%%s") do
		line = string.gsub(line, c, '')
	end
	line = " " .. line
	return utils.right_align(line, line_count_string, ' ')
	-- return utils.right_align(" " .. line , "↝" .. line_count, " ")
	-- local width = vim.api.nvim_win_get_width(0)
	-- local fill_width = width - string.len(line) - string.len(tostring(line_count)) - 4
	-- fill_width = fill_width - string.len(tostring(vim.api.nvim_buf_line_count(0)))
	-- if vim.wo.signcolumn == "yes" then fill_width = fill_width - 2 end
	-- local line_fill = string.rep(' ', fill_width)
	-- return " " .. line .. line_fill .. "↝" .. line_count
end

opt.foldtext = 'v:lua.foldtext_right_align()'
opt.fillchars = { eob = "-", fold = " " }
