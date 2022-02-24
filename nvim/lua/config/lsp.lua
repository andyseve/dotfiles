-- Author: Anish Sevekari
-- Last Modified: Fri 18 Feb 2022 05:21:17 PM EST
-- lsp config

local present, lsp = pcall(require,'lspconfig')

if not present then
	return
end


local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local on_attach = function(client,bufnr)
	local function buf_set_keymap(...) return vim.api.nvim_buf_set_keymap(bufnr,...) end
	local function buf_set_option(...) return vim.api.nvim_buf_set_option(bufnr,...) end

	buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")
	local msg = string.format("Language server %s started!", client.name)

	-- mappings
	local opts = { noremap = true, silent = true }
end

lsp.ccls.setup {
	on_attach = on_attach;
	capabilities = capabilities;
	init_options = {
		compilationDatabaseDirectory = "build";
		index = {
			threads = 0;
		};
		clang = {
			excludeArgs = { "-frounding-math" };
		};
	}
}

-- lspkind settings for better symbols

local lspkind = require('lspkind')

lspkind.init({
	mode = 'symbol_text',
	preset = 'default',
})

-- set lsp symbols (from nvchad - https://github.com/NvChad/NvChad)
local function lspSymbol(name, icon)
	local hl = "DiagnosticSign" .. name
	vim.fn.sign_define(hl, { text = icon, numhl = hl, texthl = hl })
end

lspSymbol("Error", "")
lspSymbol("Info" , "")
lspSymbol("Hint" , "")
lspSymbol("Warn" , "")

vim.diagnostic.config {
	virtual_text = {
		prefix = "",
	},
	signs = true,
	underline = true,
	update_in_insert = false,
}

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = "single",
})
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
	border = "single",
})
