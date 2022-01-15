-- Author: Anish Sevekari
-- Last Modified: Sat Jan 15 16:41:34 2022
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
	--vim.notify(msg, 'info', {title = 'LSP info'})
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
	with_text = true,
	preset = 'default',
})
