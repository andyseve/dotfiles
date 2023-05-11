-- Author: Anish Sevekari
-- Last Modified: Thu 23 Feb 2023 11:19:04 PM EST
-- # noice settings

local plugins = require("core.user").plugins
local present, noice = pcall(require, "noice")

if not present then
	return
end

local highlights = require("noice.config.highlights")

noice.setup({
	views = {
		--@see section on views
		cmdline_popup = {
			position = {
				row = 5,
				col = "50%",
			},
			size = {
				width = 60,
				height = "auto",
			},
		},
	},
	routes = {
		{
			filter = { event = "msg_show", min_height = 20 },
			view = "split",
			opts = { enter = true }
		},
	},
	format = { --- @see section on formatting
		-- default format
		default = { "{level} ", "{title} ", "{message}" },
		-- default format for vim.notify views
		notify = { "{message}" },
		-- default format for the history
		details = {
			"{date} ",
			"{level} ",
			"{event}",
			{ "{kind}", before = { ".", hl_group = "NoiceFormatKind" } },
			" ",
			"{title} ",
			"{cmdline} ",
			"{message}",
		},
	},
	throttle = 1000 / 30,
	health = {
		checker = true, -- Disable if you don't want health checks to run
	},
	smart_move = {
		-- noice tries to move out of the way of existing floating windows.
		enabled = true, -- you can disable this behaviour here
		-- add any filetypes here, that shouldn't trigger smart move.
		excluded_filetypes = { "cmp_menu", "cmp_docs", "notify" },
	},

	cmdline = {
		enabled = true,
		format = {
			-- conceal: (default=true) This will hide the text in the cmdline that matches the pattern.
			-- view: (default is cmdline view)
			-- opts: any options passed to the view
			-- icon_hl_group: optional hl_group for the icon
			-- title: set to anything or empty string to hide
			cmdline = {
				pattern = "^:",
				icon = "",
				lang = "vim"
			},
			search_down = {
				view = "cmdline",
				kind = "search",
				pattern = "^%/",
				icon = " ",
				lang = "regex"
			},
			search_up = {
				view = "cmdline",
				kind = "search",
				pattern = "^%?",
				icon = " ",
				lang = "regex"
			},
			replace = {
				view = "cmdline_popup",
				kind = "replace",
				title = " Replace ",
				icon_hl_group = highlights.defaults.CmdlineIconSearch,
				opts = {
					position = {
						row = -3,
						col = "50%",
					},
					size = {
						width = 60,
						height = "auto",
					},
					win_options = {
						winhighlight = { FloatBorder = highlights.defaults.CmdlinePopupBorderSearch },
					}
				},
				pattern = {
					"^:%%s%/",
					"^:'<,'>s%/",
					"^:%d*s%/",
					"^:%d*,%d*s%/",
					"^:%%s%?",
					"^:'<,'>s%?",
					"^:%d*s%?",
					"^:%d*,%d*s%?",
				},
				icon = "﯒ ",
				lang = "regex"
			},
			shell = {
				kind = "shell",
				pattern = "^:%s*!",
				icon = "$",
				lang = "bash"
			},
			lua = {
				pattern = "^:%s*lua%s+",
				icon = "",
				lang = "lua"
			},
			help = {
				pattern = "^:%s*he?l?p?%s+",
				icon = ""
			},
			input = {}, -- Used by input()
			-- lua = false, -- to disable a format, set to `false`
		},
	},
	messages = {
		-- NOTE: If you enable messages, then the cmdline is enabled automatically.
		-- This is a current Neovim limitation.
		enabled = true, -- enables the Noice messages UI
		view = plugins.notify and "notify" or "mini", -- default view for messages
		view_error = plugins.notify and "notify" or "mini", -- view for errors
		view_warn = plugins.notify and "notify" or "mini", -- view for warnings
		view_history = "messages", -- view for :messages
		view_search = "mini", -- view for search count messages. Set to `false` to disable
	},
	popupmenu = {
		enabled = true, -- enables the Noice popupmenu UI
		---@type 'nui'|'cmp'
		backend = "cmp", -- backend to use to show regular cmdline completions
		---@type NoicePopupmenuItemKind|false
		-- Icons for completion item kinds (see defaults at noice.config.icons.kinds)
		kind_icons = {}, -- set to `false` to disable icons
	},
	-- You can add any custom commands below that will be available with `:Noice command`
	---@type table<string, NoiceCommand>
	commands = {
		history = {
			-- options for the message history that you get with `:Noice`
			view = "split",
			opts = { enter = true, format = "details" },
			filter = {
				any = {
					{ event = "notify" },
					{ error = true },
					{ warning = true },
					{ event = "msg_show", kind = { "" } },
					{ event = "lsp", kind = "message" },
				},
			},
		},
		-- :Noice last
		last = {
			view = "split",
			opts = { enter = true, format = "details", size = 3 },
			filter = {
				any = {
					{ event = "notify" },
					{ error = true },
					{ warning = true },
					{ event = "msg_show", kind = { "" } },
					{ event = "lsp", kind = "message" },
				},
			},
			filter_opts = { count = 1 },
		},
		-- :Noice errors
		errors = {
			-- options for the message history that you get with `:Noice`
			view = "split",
			opts = { enter = true, format = "details" },
			filter = { error = true },
			filter_opts = { reverse = true },
		},
	},
	notify = {
		-- Noice can be used as `vim.notify` so you can route any notification like other messages
		-- Notification messages have their level and other properties set.
		-- event is always "notify" and kind can be any log level as a string
		-- The default routes will forward notifications to nvim-notify
		-- Benefit of using Noice for this is the routing and consistent history view
		enabled = plugins.notify,
		view = "notify",
	},
	lsp = {
		enabled = plugins.lsp,
		progress = {
			enabled = true,
			-- Lsp Progress is formatted using the builtins for lsp_progress. See config.format.builtin
			-- See the section on formatting for more details on how to customize.
			--- @type NoiceFormat|string
			format = "lsp_progress",
			--- @type NoiceFormat|string
			format_done = "lsp_progress_done",
			throttle = 1000 / 30, -- frequency to update lsp progress message
			view = "mini",
		},
		override = {
			-- override the default lsp markdown formatter with Noice
			["vim.lsp.util.convert_input_to_markdown_lines"] = true,
			-- override the lsp markdown formatter with Noice
			["vim.lsp.util.stylize_markdown"] = true,
			-- override cmp documentation with Noice (needs the other options to work)
			["cmp.entry.get_documentation"] = true,
		},
		hover = {
			enabled = true,
			view = nil, -- when nil, use defaults from documentation
			---@type NoiceViewOptions
			opts = {}, -- merged with defaults from documentation
		},
		signature = {
			enabled = not plugins.lspsignature,
			auto_open = {
				enabled = true,
				trigger = true, -- Automatically show signature help when typing a trigger character from the LSP
				luasnip = plugins.luasnip, -- Will open signature help when jumping to Luasnip insert nodes
				throttle = 50, -- Debounce lsp signature help request by 50ms
			},
			view = nil, -- when nil, use defaults from documentation
			---@type NoiceViewOptions
			opts = {}, -- merged with defaults from documentation
		},
		message = {
			-- Messages shown by lsp servers
			enabled = true,
			view = "notify",
			opts = {},
		},
		-- defaults for hover and signature help
		documentation = {
			view = "hover",
			---@type NoiceViewOptions
			opts = {
				lang = "markdown",
				replace = true,
				render = "plain",
				format = { "{message}" },
				win_options = { concealcursor = "n", conceallevel = 3 },
			},
		},
	},
	markdown = {
		hover = {
			["|(%S-)|"] = vim.cmd.help, -- vim help links
			["%[.-%]%((%S-)%)"] = require("noice.util").open, -- markdown links
		},
		highlights = {
			["|%S-|"] = "@text.reference",
			["@%S+"] = "@parameter",
			["^%s*(Parameters:)"] = "@text.title",
			["^%s*(Return:)"] = "@text.title",
			["^%s*(See also:)"] = "@text.title",
			["{%S-}"] = "@parameter",
		},
	},
})
