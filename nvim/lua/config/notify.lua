-- Author: Anish Sevekari
-- Last Modified: Sat Jan 15 16:48:27 2022
-- # nvim-notify settings

local present, notify = pcall(require,'notify')

if not present then
	return
end

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
