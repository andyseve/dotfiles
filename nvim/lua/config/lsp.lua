-- Author: Anish Sevekari
-- Last Modified: Fri 03 Mar 2023 03:21:17 PM EST
-- lsp config

local utils = require("core.utils")

local present, lsp = pcall(require, "lspconfig")
if not present then
	return
end

-- keymaps
local lsp_keymaps = function(bufnr)
	local opts = { noremap = true, silent = true }
	local keymap = vim.api.nvim_buf_set_keymap
	keymap(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
	keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
	keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
	keymap(bufnr, "n", "gI", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
	keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
	keymap(bufnr, "n", "gl", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
end

-- TODO: Add this into a mapping table.
-- keymap(bufnr, "n", "<leader>df", "<cmd>lua vim.lsp.buf.format{ async = true }<cr>", opts)
-- keymap(bufnr, "n", "<leader>di", "<cmd>LspInfo<cr>", opts)
-- keymap(bufnr, "n", "<leader>dI", "<cmd>LspInstallInfo<cr>", opts)
-- keymap(bufnr, "n", "<leader>da", "<cmd>lua vim.lsp.buf.code_action()<cr>", opts)
-- keymap(bufnr, "n", "<leader>dj", "<cmd>lua vim.diagnostic.goto_next({buffer=0})<cr>", opts)
-- keymap(bufnr, "n", "<leader>dk", "<cmd>lua vim.diagnostic.goto_prev({buffer=0})<cr>", opts)
-- keymap(bufnr, "n", "<leader>dr", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
-- keymap(bufnr, "n", "<leader>ds", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
-- keymap(bufnr, "n", "<leader>dq", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
-- key("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
-- key("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, bufopts)
-- key("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, bufopts)
-- key("n", "<leader>wl", function()
--print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
--end, bufopts)

-- on attach function
local on_attach = function(client, bufnr)
	-- Mappings
	lsp_keymaps(bufnr)

	-- Enable completion triggered by <c-x><c-o>
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
	local msg = string.format("Language server %s started!", client.name)
	utils.inspect(msg)
end

-- capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem = {
	documentationFormat = { "markdown", "plaintext" },
	snippetSupport = true,
	preselectSupport = true,
	insertReplaceSupport = true,
	labelDetailsSupport = true,
	deprecatedSupport = true,
	commitCharactersSupport = true,
	tagSupport = { valueSet = { 1 } },
	resolveSupport = {
		properties = {
			"documentation",
			"detail",
			"additionalTextEdits",
		},
	},
}

-- include capabilities provided by nvim_cmp
local present_cmp_nvim_lsp, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if present_cmp_nvim_lsp then
	capabilities = cmp_nvim_lsp.default_capabilities(capabilities)
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

lsp.lua_ls.setup {
	on_attach = on_attach,
	capabilities = capabilities,
	flags = lsp_flags,
	single_file_support = true,
	settings = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
				version = 'LuaJIT',
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { 'vim' },
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file("", true),
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
		},
	},
}

lsp.rust_analyzer.setup {
	on_attach = on_attach,
	capabilities = capabilities,
	flags = lsp_flags,
}
