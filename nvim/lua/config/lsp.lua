-- Author: Anish Sevekari
-- Last Modified: Fri Oct 28 13:22:28 2022
-- lsp config

local present, lsp = pcall(require,'lspconfig')

if not present then
	return
end


local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

-- include capabilities probided by nvim_cmp
local present, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
if present then
	capabilities = cmp_nvim_lsp.default_capabilities(capabilities)
end


-- lspkind settings for better symbols
local present, lspkind = pcall(require,'lspkind')

if present then
	lspkind.init({
		mode = 'symbol_text',
		preset = 'default',
	})
end

-- set lsp symbols (from https://github.com/LunarVim/Neovim-from-scratch)
local signs = {
	{ name = "DiagnosticSignError", text = "" },
	{ name = "DiagnosticSignWarn", text = "" },
	{ name = "DiagnosticSignHint", text = "" },
	{ name = "DiagnosticSignInfo", text = "" },
}

for _, sign in ipairs(signs) do
	vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

vim.diagnostic.config({
	virtual_text = false,
	signs = {
		active = signs,
	},
	underline = true,
	update_in_insert = false,
	severity_sort = true,
	float = {
		focusable = false,
		style = "minimal",
		border = "rounded",
		source = "always",
		header = "",
		prefix = "",
	},
})

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = "rounded",
})
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
	border = "rounded",
})

-- keymaps
local function lsp_keymaps(bufnr)
	local opts = { noremap = true, silent = true }
	local keymap = vim.api.nvim_buf_set_keymap
	keymap(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
	keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
	keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
	keymap(bufnr, "n", "gI", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
	keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
	keymap(bufnr, "n", "gl", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
	-- Change l mappings if they conflict with latex
	keymap(bufnr, "n", "<leader>lf", "<cmd>lua vim.lsp.buf.format{ async = true }<cr>", opts)
	keymap(bufnr, "n", "<leader>li", "<cmd>LspInfo<cr>", opts)
	keymap(bufnr, "n", "<leader>lI", "<cmd>LspInstallInfo<cr>", opts)
	keymap(bufnr, "n", "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<cr>", opts)
	keymap(bufnr, "n", "<leader>lj", "<cmd>lua vim.diagnostic.goto_next({buffer=0})<cr>", opts)
	keymap(bufnr, "n", "<leader>lk", "<cmd>lua vim.diagnostic.goto_prev({buffer=0})<cr>", opts)
	keymap(bufnr, "n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
	keymap(bufnr, "n", "<leader>ls", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
	keymap(bufnr, "n", "<leader>lq", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
end

-- on attach function

local on_attach = function(client,bufnr)
	lsp_keymaps(bufnr)
	buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")
	local msg = string.format("Language server %s started!", client.name)
end

-- lsp flags
local lsp_flags = {
	debounce_text_changes = 150,
}

-- language server settings

lsp.ccls.setup {
	on_attach = on_attach,
	capabilities = capabilities,
	flags = lsp_flags,
	single_file_support = true,
	init_options = {
		compilationDatabaseDirectory = "build",
		index = {
			threads = 0,
		},
		clang = {
			excludeArgs = { "-frounding-math" },
		},
	},
}

lsp.hls.setup {
	on_attach = on_attach,
	capabilities = capabilities,
	flags = lsp_flags,
	single_file_support = true,
}

lsp.pyright.setup {
	on_attach = on_attach,
	capabilities = capabilities,
	flags = lsp_flags,
	single_file_support = true,
}



