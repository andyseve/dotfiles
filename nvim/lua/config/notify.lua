-- Author: Anish Sevekari
-- Last Modified: Thu 07 Oct 2021 02:44:47 AM EDT
-- # nvim-notify settings

local notify = require('notify')

notify.setup({
	stages = "fade_in_slide_out",
	timeout = 5000,
	icons = {
		ERROR = "",
		WARN = "",
		INFO = "",
		DEBUG = "",
		TRACE = "✎",
	},
})

--vim.notify = notify
