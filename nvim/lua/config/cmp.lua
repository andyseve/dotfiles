-- Author: Anish Sevekari
-- Last Modified: Tue 07 Mar 2023 04:07:30 AM EST
-- nvim-cmp settings

local utils = require("core.utils")
local plugins = require("core.user").plugins
local cmp_present, cmp = pcall(require, 'cmp')
local lspkind_present, lspkind = pcall(require, 'lspkind')

if not cmp_present then
	return
end

local sources = function(omni)
	local ret = {}
	if plugins.lsp then table.insert(ret, { name = "nvim_lsp" }) end
	if plugins.ultisnips then table.insert(ret, { name = "ultisnips" }) end
	if plugins.luasnip then table.insert(ret, { name = "luasnip" }) end
	table.insert(ret, { name = "path" })
	if omni then
		table.insert(ret, { name = "omni" })
	end
	table.insert(ret, { name = "buffer" })
	return ret
end

local snippet_sources = function()
	local ret = { config = { sources = {} } }
	if plugins.ultisnips then table.insert(ret.config.sources, { name = "ultisnips" }) end
	if plugins.luasnip then table.insert(ret.config.sources, { name = "luasnip" }) end
	return ret
end

local cmp_next = function(fallback)
	if cmp.visible() then
		cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
	else
		fallback()
	end
end
local cmp_prev = function(fallback)
	if cmp.visible() then
		cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
	else
		fallback()
	end
end
local cmp_confirm = function(fallback)
	if cmp.visible() and cmp.get_active_entry() then
		cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true })
	else
		fallback()
	end
end

-- snippet dependent features
-- utils.cmp provides keybind functions depending on snippet engine of choice.
-- If the snippet engine is changed, then we need to reselect the appropriate functions.
-- Note that tab bindings insert the completion by default, on the other hand, other keybinds only select the appropriate choice without inserting it.
-- The idea is to use tabs for uninturpted typing only, and in all other cases, it is advised to use other keybinds to select, rather than schrolling to the correction option.
-- Fuzzyfind helps with this, since completing the required word should automatically bring up the preffered choice to the top.

local snippet_next_insert
if plugins.ultisnips then
	snippet_next_insert = function(fallback)
		if vim.fn["UltiSnips#CanExpandSnippet"]() == 1 then
			vim.fn["UltiSnips#ExpandSnippet"]()
		elseif vim.fn["UltiSnips#CanJumpForwards"]() == 1 then
			vim.fn["UltiSnips#JumpForwards"]()
		elseif cmp.visible() then
			cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
		else
			fallback()
		end
	end
else
	snippet_next_insert = cmp_next
end


local snippet_next_select
if plugins.ultisnips then
	snippet_next_select = function(fallback)
		if plugins.ultisnips then
			if vim.fn["UltiSnips#CanJumpForwards"]() == 1 then
				vim.api.nvim_feedkeys(utils.termcodes("<ESC>:call UltiSnips#JumpForwards()<CR>"), 'm', true)
			else
				fallback()
			end
			return
		end
	end
else
	snippet_next_select = cmp_next
end

local snippet_prev_insert
if plugins.ultisnips then
	snippet_prev_insert = function(fallback)
		if vim.fn["UltiSnips#CanJumpBackwards"]() == 1 then
			vim.fn["UltiSnips#JumpBackwards"]()
		elseif cmp.visible() then
			cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
		else
			fallback()
		end
	end
else
	snippet_next_insert = cmp_next
end

local snippet_prev_select
if plugins.ultisnips then
	snippet_prev_select = function(fallback)
		if vim.fn["UltiSnips#CanJumpBackwards"]() == 1 then
			vim.api.nvim_feedkeys(utils.termcodes("<ESC>:call UltiSnips#JumpBackwards<CR>"), 'm', true)
		else
			fallback()
		end
	end
else
	snippet_prev_select = cmp_prev
end

cmp.setup({
	enabled = plugins.nvimcmp,
	snippet = {
		expand = function(args)
			vim.fn["UltiSnips#Anon"](args.body)
		end,
	},
	window = {
		completion = cmp.config.window.bordered({
			border = "rounded",
		}),
		documentation = cmp.config.window.bordered({
			border = "rounded",
			max_height = 10,
		}),
	},
	mapping = {
		["<DOWN>"] = cmp.mapping({
			i = cmp_next,
			s = cmp.select_next_item({ behavior = cmp.SelectBehavior.Select }),
			c = cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
		}),
		["<UP>"] = cmp.mapping({
			i = cmp_prev,
			s = cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
			c = cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
		}),
		["<c-j>"] = cmp.mapping({
			i = cmp_next,
			s = cmp.select_next_item({ behavior = cmp.SelectBehavior.Select }),
			c = cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
		}),
		["<c-k>"] = cmp.mapping({
			i = cmp_prev,
			s = cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
			c = cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
		}),
		["<c-n>"] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Replace }), { "i", "c" }),
		["<c-p>"] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Replace }), { "i", "c" }),
		["<c-b>"] = cmp.mapping.scroll_docs(-4),
		["<c-f>"] = cmp.mapping.scroll_docs(4),
		["<c-Space>"] = cmp.mapping.complete(),
		["<c-l>"] = cmp.mapping.complete(snippet_sources()),
		["<c-e>"] = cmp.mapping.abort(),
		["<c-y>"] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
		["<CR>"] = cmp.mapping({
			i = cmp_confirm,
			s = cmp.confirm({ select = true }),
			c = cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
		}),
		["<Tab>"] = cmp.mapping({
			c = function()
				if cmp.visible() then
					cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
				else
					cmp.complete()
				end
			end,
			i = snippet_next_insert,
			s = snippet_next_select
		}),
		["<S-Tab>"] = cmp.mapping({
			c = function()
				if cmp.visible() then
					cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
				else
					cmp.complete()
				end
			end,
			i = snippet_prev_insert,
			s = snippet_prev_select
		}),
	},
	formatting = {
		format = lspkind.cmp_format({
			mode = "symbol",
			preset = "codicons",
			maxwidth = 50,
			ellipsis_char = "...",
		}),
	},
	-- list of sources to add to lsp
	sources = cmp.config.sources(sources(false)),
	experimental = {
		native_menu = false,
		ghost_text = true,
	},
})

-- Filetype specific sources
cmp.setup.filetype('gitcommit', {
	sources = cmp.config.sources({
		{ name = 'git' },
		{ name = 'buffer' },
	})
})
cmp.setup.filetype('tex', {
	completion = {
		keyword_length = 1,
		completeopt = "menu, noselect",
	},
	sources = cmp.config.sources(sources(true))
})

-- Use buffer source for `/`, '?'
cmp.setup.cmdline({ '/', '?' }, {
	completion = {
		keyword_length = 3,
	},
	view = {
		entries = {
			name = 'wildmenu',
			separator = '|'
		},
	},
	sources = {
		{ name = 'buffer' },
	},
})

-- Use cmdline & path source for ':'.
cmp.setup.cmdline(':', {
	view = {
		entries = {
			name = "native_menu"
		}
	},
	sources = {
		{ name = 'path' },
		{ name = 'cmdline' }
	},
})
