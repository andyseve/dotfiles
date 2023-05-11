local ok_trouble, trouble = pcall(require, "trouble")
if not ok_trouble then
	return
end

trouble.setup()

key("n", "<leader>xx", "<cmd>TroubleToggle<cr>", opts)
key("n", "<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>", opts)
key("n", "<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>", opts)
key("n", "<leader>xl", "<cmd>TroubleToggle loclist<cr>", opts)
key("n", "<leader>xq", "<cmd>TroubleToggle quickfix<cr>", opts)
key("n", "<leader>gr", "<cmd>TroubleToggle lsp_references<cr>", opts)
