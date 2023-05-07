local utils = require("core.utils")

local config = {
	background = {
		light = "latte",
		dark = "mocha",
	},
	term_colors = true,
	integrations = {
		cmp = true,
		nvimtree = true,
		telescope = true,
		notify = true,
		leap = true,
		treesitter = true,
		treesitter_context = true,
		indent_blankline = {
			enabled = true,
			colored_indent_levels = true,
		},
		native_lsp = {
			enabled = true,
			virtual_text = {
				errors = { "italic" },
				hints = { "italic" },
				warnings = { "italic" },
				information = { "italic" },
			},
			underlines = {
				errors = { "underline" },
				hints = { "underline" },
				warnings = { "underline" },
				information = { "underline" },
			},
		},
		lsp_saga = true,
		which_key = true,
		markdown = true,
	},
	transparent_background = false,
	dim_inactive = {
		enabled = false,
		shade = "dark",
		percentage = 0.15,
	},
	styles = {
		comments = { "italic" },
		conditionals = { "italic" },
		loops = { "bold" },
		functions = {},
		keywords = { "bold" },
		strings = {},
		variables = {},
		numbers = {},
		booleans = { "bold" },
		properties = {},
		types = { "bold" },
		operators = {},
	},
}

local create_catppuccin_theme = function(theme)
	if theme == nil then
		utils.inspect("No theme selected, using default")
		return config
	end


	local themefile = "config.themes." .. theme
	local theme_exists, themeconfig = pcall(require, themefile)
	if not theme_exists then
		utils.inspect("Choosen theme file " .. themefile .. " does not exist")
		return config
	end
	if type(themeconfig) ~= "table" then
		utils.inspect("Choosen theme file " .. themefile .. " contains invalid theme")
		return config
	end

	local merged_config = vim.tbl_deep_extend("force", config, themeconfig)
	merged_config.color_overrides = vim.deepcopy(themeconfig.colors)
	merged_config.colors = nil
	if config.custom_highlights ~= nil and themeconfig.custom_highlights ~= nil then
		merged_config.custom_highlights = function(colors)
			return vim.tbl_deep_extend("force", config.custom_highlights(colors), themeconfig.custom_highlights(colors))
		end
	end
	utils.log(merged_config)
	return merged_config
end

local set_catppuccin_theme = function(theme)
	local ok, catppuccin = pcall(require, "catppuccin")
	if not ok then
		vim.inspect("Catppuccin not installed.")
		return
	end
	catppuccin.setup(create_catppuccin_theme(theme))
	vim.cmd.colorscheme("catppuccin")
end


vim.api.nvim_create_user_command(
	'SetCatppuccinTheme',
	function(opts)
		set_catppuccin_theme(opts.fargs[1])
	end,
	{
		nargs = 1,
		complete = function(ArgLead, CmdLine, CursorPos)
			-- return completion candidates as a list-like table
			return { "catppuccin", "solarized", "custom", "wal", "sahyadri" }
		end,
	}
)

local theme = require("core.user").theme
set_catppuccin_theme(theme)
