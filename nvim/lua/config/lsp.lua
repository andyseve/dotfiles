local lsp = require('lspconfig')

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

lsp.ccls.setup {
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

local lspkind = require('lspkind')

lspkind.init({
	with_text = true,
	preset = 'default',
})
