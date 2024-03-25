-- Author: Anish Sevekari
-- Last Modified: Fri 03 Mar 2023 03:21:17 PM EST
-- lsp config

local user = require("core.user")
local utils = require("core.utils")
local present, saga = pcall(require, "lspsaga")
if not present then
	return
end

local config = {
	ui = {
		-- This option only works in Neovim 0.9
		title = true,
		-- Border type can be single, double, rounded, solid, shadow.
		border = "single",
		winblend = 0,
		expand = "",
		collapse = "",
		code_action = " ",
		incoming = " ",
		outgoing = " ",
		hover = " ",
	},
	hover = {
		max_width = 0.6,
		open_link = 'gx',
		open_browser = '!firefox',
	},
	finder = {
		max_height = 0.5,
		min_width = 30,
		force_max_height = false,
		keys = {
			jump_to = 'p',
			expand_or_jump = 'o',
			vsplit = 's',
			split = 'i',
			tabe = 't',
			tabnew = 'r',
			quit = { 'q', '<ESC>' },
			close_in_preview = '<ESC>',
		},
	},
	definition = {
		edit = "<C-c>o",
		vsplit = "<C-c>v",
		split = "<C-c>i",
		tabe = "<C-c>t",
		quit = "q",
	},
	rename = {
		quit = "<C-c>",
		exec = "<CR>",
		mark = "x",
		confirm = "<CR>",
		in_select = true,
	},
	outline = {
		win_position = "right",
		win_with = "",
		win_width = 30,
		preview_width = 0.4,
		show_detail = true,
		auto_preview = true,
		auto_refresh = true,
		auto_close = true,
		custom_sort = nil,
		keys = {
			expand_or_jump = 'o',
			quit = "q",
		},
	},
	code_action = {
		num_shortcut = true,
		show_server_name = true,
		extend_gitsigns = true,
		keys = {
			-- string | table type
			quit = "q",
			exec = "<CR>",
		},
	},
	lightbulb = {
		enable = true,
		enable_in_insert = true,
		sign = true,
		sign_priority = 40,
		virtual_text = true,
	},
	callhierarchy = {
		show_detail = false,
		keys = {
			edit = "e",
			vsplit = "s",
			split = "i",
			tabe = "t",
			jump = "o",
			quit = "q",
			expand_collapse = "u",
		},
	},
	symbol_in_winbar = {
		enable = true,
		separator = " ",
		ignore_patterns = {},
		hide_keyword = true,
		show_file = true,
		folder_level = 2,
		respect_root = false,
		color_mode = true,
	},
}

if user.use_catppuccin then
	config.ui = {
		colors = require("catppuccin.groups.integrations.lsp_saga").custom_colors(),
		kind = require("catppuccin.groups.integrations.lsp_saga").custom_kind(),
	}
end

saga.setup(config)

local lspsaga_prev_error = function()
	require("lspsaga.diagnostic"):goto_prev({ severity = vim.diagnostic.severity.ERROR })
end

local lspsaga_next_error = function()
	require("lspsaga.diagnostic"):goto_next({ severity = vim.diagnostic.severity.ERROR })
end

local mappings = {}
mappings.lsp_general = {
	whichkey = false,
	name = "lspsaga",
	n = {
		["gh"] = { "<cmd> Lspsaga lsp_finder <CR>", " lsp_finder" },
		["gr"] = { "<cmd> Lspsaga rename <CR>", " refactor" },
		["gR"] = { "<cmd> Lspsaga rename ++project <CR>", " refactor" },
		["gd"] = { "<cmd> Lspsaga goto_definition <CR>", " goto definition" },
		["gp"] = { "<cmd> Lspsaga peek_definition <CR>", " peek definition" },
		["[e"] = { "<cmd> Lspsaga diagnostic_jump_prev <CR>", " warning prev" },
		["]e"] = { "<cmd> Lspsaga diagnostic_jump_next <CR>", " warning next" },
		["[E"] = { lspsaga_prev_error, " error prev" },
		["]E"] = { lspsaga_next_error, " error next" },
		["K"] = { "<cmd> Lspsaga hover_doc <CR>", " Hover" },
	}
}
mappings.diagnostics = {
	name = " Diagnostics",
	prefix = "<leader>d",
	n = {
		["l"] = { "<cmd> Lspsaga show_line_diagnostics <CR>", " Line diagnostics" },
		["c"] = { "<cmd> Lspsaga show_cursor_diagnostics <CR>", " Cursor diagnostics" },
		["b"] = { "<cmd> Lspsaga show_buf_diagnostics <CR>", " Buffer diagnostics" },
	}
}

mappings.lists = {
	name = " Lists",
	prefix = "<leader>o",
	n = {
		["e"] = { "<cmd> Lspsaga outline <CR>", "﬌ Code outline" },
		["c"] = { "<cmd> Lspsaga incoming_calls <CR>", " Incoming calls" },
		["v"] = { "<cmd> Lspsaga outgoing_calls <CR>", " Outgoing calls" },
	}
}

utils.load_mappings(mappings)

-- key({ "n", "v" }, "<leader>ca", "<cmd>Lspsaga code_action<CR>")
