-- Author: Anish Sevekari
-- Last Modified: Tue 25 Jan 2022 05:22:19 PM EST
-- lsp signature config

local present, lsp_signature = pcall(require,'lsp_signature')

if not present then
	return
end

lsp_signature.setup({
	bind = true,
	doc_lines = 10,

	floating_window_off_x = 1,
	floating_window_off_y = 1,

	hint_enable = true,
	hint_prefix = "🐼 ",  -- Panda for parameter
  hint_scheme = "String",
  hi_parameter = "LspSignatureActiveParameter", -- how your parameter will be highlight
  max_height = 12, -- max height of signature floating_window, if content is more than max_height, you can scroll down to view the hiding contents
  max_width = 80, -- max_width of signature floating_window, line will be wrapped if exceed max_width
  handler_opts = {
    border = "rounded"   -- double, rounded, single, shadow, none
  },
})

