-- luacheck: globals vim
local present, notify = pcall(require, "notify")
if not present then
	return
end

notify.setup({
	stages = "fade_in_slide_out",
	timeout = 2000,
})

vim.notify = notify
