-- Mapping for nvim
-- Author: Anish Sevekari
-- Last Modified: Mon 06 Mar 2023 11:09:57 PM EST

-- n, v, i are mode names
-- supports whichkey through load_mappings defined in utils

vim.g.mapleader = "\\"

local utils = require("core.utils")
local plugins = require("core.user").plugins

local mappings = {}

mappings.general = {
	whichkey = false,
	i = {
		-- save
		["<c-s>"] = { "<cmd> update <CR>", "﬚ save file" },

		-- delete
		["<c-backspace>"] = { "<cmd> dbx", " backspace" },

		-- search and replace
		["<c-r>"] = { "<c-o>:%s/", "﯒ replace" },
	},
	n = {
		-- highlight
		["<ESC>"] = { "<cmd> noh <CR>", " no highlight" },

		-- movement
		["j"] = { "gj", " move down" },
		["k"] = { "gk", " move up" },

		-- switch between windows
		["<c-h>"] = { "<C-w>h", " window left" },
		["<c-l>"] = { "<C-w>l", " window right" },
		["<c-j>"] = { "<C-w>j", " window down" },
		["<c-k>"] = { "<C-w>k", " window up" },

		-- save
		["<c-s>"] = { "<cmd> update <CR>", "﬚ save file" },

		-- copy paste
		["<c-c>"] = { "<cmd> %y+ <CR>", " copy whole file" },
		["<leader>y"] = { "\"+y", " copy" },
		["<leader>p"] = { "\"+p", " paste" },

		-- delete
		["<c-backspace>"] = { "<cmd> dbx", " backspace" },

		-- search and replace
		["<c-r>"] = { ":%s/", "﯒ replace" },
	},
	v = {
		-- movement
		["j"] = { "gj", " move down" },
		["k"] = { "gk", " move up" },
		["J"] = { ":m '>+1<CR>gv=gv", " shift down" },
		["K"] = { ":m '<-2<CR>gv=gv", " shift up" },

		-- copy paste
		["<leader>y"] = { "\"+y", " copy" },
		["<leader>p"] = { "\"+p", " paste" },

		-- search and replace
		["<c-r>"] = { ":s/", "﯒ replace" },
	}
}

mappings.edit = {
	name = " Edit",
	prefix = "<leader>e",
	n = {
		["v"] = { "<cmd> vsplit $MYVIMRC <CR>", " nvim" },
		["l"] = { "<cmd> vsplit ~/dotfiles/latex/anishs.sty <CR>", " latex" },
		["z"] = { "<cmd> vsplit ~/.config/zsh/.zshrc", " zsh" },
	}
}

mappings.source = {
	whichkey = false,
	n = {
		["sv"] = { "<cmd> source $MYVIMRC <CR>", " vimrc" },
	}
}


mappings.lists = {
	name = " Lists",
	prefix = "<leader>o",
	n = {
		["o"] = { "<cmd> cclose <CR>", " Quickfix" },
		["k"] = { "<cmd> copen <CR>", " Quickfix" },
		["i"] = { "<cmd> lwindow <CR>", " Locations" },
	}
}

if not plugins.telescope then
	mappings.lists.n["w"] = { "<cmd> lwindow <CR>", " Locations" }
	mappings.litst.n["q"] = { "<cmd> cwindow <CR>", " Quickfix" }
end

mappings.windows = {
	name = " indows",
	prefix = "<leader>w",
	n = {
		["t"] = { "<cmd> bnext <CR>", " Buffer" },
		["T"] = { "<cmd> bprev <CR>", " Buffer" },
		["p"] = { "<cmd> split <CR>", "══ Split" },
		["o"] = { "<cmd> vsplit <CR>", " ║ Split" },
		["i"] = { "<cmd> close <CR>", " Close" }
	}
}

mappings.buffers = {
	name = " Buffers",
	prefix = "<leader>b",
	n = {
		["n"] = { "<cmd> bnext <CR>", " Buffer" },
		["N"] = { "<cmd> bprev <CR>", " Buffer" },
		["P"] = { "<cmd> bnext <CR>", " Buffer" },
		["p"] = { "<cmd> bprev <CR>", " Buffer" },
	}
}

utils.load_mappings(mappings)

return mappings
