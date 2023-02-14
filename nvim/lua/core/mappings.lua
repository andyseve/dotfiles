-- Mapping for nvim
-- Author: Anish Sevekari
-- Last Modified: Sat 04 Jun 2022 07:08:29 PM EDT

-- n, v, i are mode names
-- supports whichkey through load_mappings defined in utils

local utils = require("core.utils")

local mappings = {}
local groups = {}

mappings.general = {
	i = {
		-- save
		["<c-s>"] = { "<cmd> update <CR>", "﬚ save file", whichkey = false },

		-- delete
		["<c-backspace>"] = { "<cmd> dbx", whichkey = false },
	},
	n = {
		-- highlight
		["<ESC>"] = { "<cmd> noh <CR>", " no highlight", whichkey = false },

		-- movement
		["j"] = { "gj" , " move down", whichkey = false },
		["k"] = { "gk" , " move up", whichkey = false },

		-- switch between windows
		["<c-h>"] = { "<C-w>h", " window left", whichkey = false },
		["<c-l>"] = { "<C-w>l", " window right", whichkey = false },
		["<c-j>"] = { "<C-w>j", " window down", whichkey = false },
		["<c-k>"] = { "<C-w>k", " window up", whichkey = false },

		-- save
		["<c-s>"] = { "<cmd> update <CR>", "﬚ save file", whichkey = false },

		-- copy paste
		["<c-c>"] = { "<cmd> %y+ <CR>", " copy whole file" },
		["<leader>y"] = { "\"+y", " copy" },
		["<leader>p"] = { "\"+p", " paste" },

		-- delete
		["<c-backspace>"] = { "<cmd> dbx", whichkey = false },
	},
	v = {
		-- movement
		["j"] = { "gj" , " move down", whichkey = false },
		["k"] = { "gk" , " move up", whichkey = false },

		-- copy paste
		["<leader>y"] = { "\"+y", " copy" },
		["<leader>p"] = { "\"+p", " paste" },
	}
}

mappings.edit = {
	n = {
		["<leader>ev"] = { "<cmd> vsplit ~/.vimrc <CR>", "vim" },
		["<leader>en"] = { "<cmd> vsplit ~/.config/nvim/init.vim <CR>", "neovim" },
		["<leader>el"] = { "<cmd> vsplit ~/dotfiles/latex/anishs.sty <CR>", "latex" },
		["<leader>ez"] = { "<cmd> vsplit ~/.zshrc", "zsh" },
		["sv"] = { "<cmd> source $MYVIMRC <CR>", "vimrc" },
	}
}
groups["edit"] = "<leader>e"

mappings.lists = {
	n = {
		["<leader>oo"] = { "<cmd> cclose <CR>", " Quickfix" },
		["<leader>ok"] = { "<cmd> copen <CR>", " Quickfix" },
		["<leader>oq"] = { "<cmd> cwindow <CR>", " Quickfix" },
		["<leader>ow"] = { "<cmd> lwindow <CR>", " Locations" },
		["<leader>ol"] = { "<cmd> lwindow <CR>", " Locations" },
	}
}
groups["lists"] = "<leader>o"

utils.load_mappings(mappings)
utils.add_mapping_groups(groups)

return mappings
