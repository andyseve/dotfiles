-- Author: Anish Sevekari
-- Last Modified: Thu 23 Feb 2023 11:19:04 PM EST
-- # treesitter settings

local present, ts_configs = pcall(require, 'nvim-treesitter.configs')

if not present then
	return
end

ts_configs.setup({
	-- A list of parser names, or "all"
	ensure_installed = {
		'c',
		'cpp',
		'python',
		'haskell',
		'lua',
		'vim',
		'nix',
		'bash',
		'latex',
		'bibtex',
		'make',
		'markdown',
		'markdown_inline',
		'help',
		'regex',
	},
	-- ignore_install = {}, -- this is only applicable if ensure_installed = all

	sync_install = true,
	auto_install = false,
	ignore_install = {},

	highlight = {
		enable = true,
		disable = function(lang, buf)
			local disable_filetypes = {
				'latex' -- treesitter conflicts vimtex
			}
			for _, ft in pairs(disable_filetypes) do
				if lang == ft then return true end
			end
			local max_filesize = 100 * 1024 -- 100 KB
			local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
			if ok and stats and stats.size > max_filesize then
				return true
			end
			return false
		end,
		additional_vim_regex_highlighting = false,
	},

	indent = {
		-- Experimental
		-- Enables indent = symbol based on treesitter
		enable = true,
	},

	matchup = {
		enable = true;
	},

	textobjects = {
		select = {
			enable = true,
			lookahead = true,
			keymaps = {
				["af"] = { query = "@function.outer", desc = "Select outer part of a function region" },
				["if"] = { query = "@function.inner", desc = "Select inner part of a function region" },
				["ac"] = { query = "@class.outer", desc = "Select outer part of a class region" },
				["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
			},
			selection_modes = {
				["@parameter.outer"] = "v", -- charwise
				["@function.outer"] = "V", -- linewise
				["@class.outer"] = "<c-v>", -- blockwise
			},
			include_surrounding_whitespace = true,
		},
	},

	refactor = {
		highlight_definitions = {
			enable = true,
			-- Set to false if you have an `updatetime` of ~100.
			clear_on_cursor_move = true,
		},
		smart_rename = {
			enable = true,
			keymaps = {
				smart_rename = "grr",
			},
		},
		highlight_current_scope = { enable = true },
		navigation = {
			enable = true,
			keymaps = {
				goto_definition = "gnd",
				list_definitions = "gnD",
				list_definitions_toc = "gO",
				goto_next_usage = "<a-*>",
				goto_previous_usage = "<a-#>",
			},
		},
	},

	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = "gnn",
			node_incremental = "grn",
			scope_incremental = "grc",
			node_decremental = "grm",
		},
	},
})

require("treesitter-context").setup({
	enable = true,
})
