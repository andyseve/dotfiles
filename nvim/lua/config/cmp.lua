-- Author: Anish Sevekari
-- Last Modified: Wed 02 Mar 2022 06:29:47 PM EST
-- nvim-cmp settings

local cmp_present, cmp = pcall(require,'cmp')
local lspkind_present, lspkind = pcall(require,'lspkind')

if not cmp_present then
	return
end

local t = function(str)
	return vim.api.nvim_replace_termcodes(str,true,true,true)
end

cmp.setup({
	snippet = {
		expand = function(args)
			vim.fn["UltiSnips#Anon"](args.body)
		end,
	},

	formatting = {
		format = function(entry, vim_item)
			if lspkind_present then
				vim_item.kind = lspkind.presets.default[vim_item.kind] .. ' ' .. vim_item.kind
			end

			vim_item.menu = ({
				nvim_lsp = "[LSP]",
				nvim_lua = "[Lua]",
				buffer = "[BUF]",
				omni = (vim.inspect(vim_item.menu):gsub('%"', "")),
				ultisnips = "[UltiSnip]"
			})[entry.source.name]

			return vim_item
		end,
	},

	sources = cmp.config.sources({
		{ name = 'ultisnips' },
		{ name = 'nvim_lsp' },
		{ name = 'nvim_lua' },
		{ name = 'omni' }
	}, {
		{ name = 'path' },
		{ name = 'buffer' },
		{ name = 'spell' },
		{ name = 'emoji' }
	}),

	mapping = {
		["<Tab>"] = cmp.mapping({
			c = function()
				if cmp.visible() then
					cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
				else
					cmp.complete()
				end
			end,
			i = function(fallback)
				if vim.fn["UltiSnips#CanExpandSnippet"]() == 1 then
					vim.api.nvim_feedkeys(t("<Plug>(ultisnips_expand)"), 'm', true)
				elseif cmp.visible() then
					cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
				elseif vim.fn["UltiSnips#CanJumpForwards"]() == 1 then
					vim.api.nvim_feedkeys(t("<Plug>(ultisnips_jump_forward)"), 'm', true)
				else
					fallback()
				end
			end,
			s = function(fallback)
				if vim.fn["UltiSnips#CanJumpForwards"]() == 1 then
					vim.api.nvim_feedkeys(t("<Plug>(ultisnips_jump_forward)"), 'm', true)
				else
					fallback()
				end
			end
		}),
		["<S-Tab>"] = cmp.mapping({
			c = function()
				if cmp.visible() then
					cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
				else
					cmp.complete()
				end
			end,
			i = function(fallback)
				if cmp.visible() then
					cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
				elseif vim.fn["UltiSnips#CanJumpBackwards"]() == 1 then
					return vim.api.nvim_feedkeys( t("<Plug>(ultisnips_jump_backward)"), 'm', true)
				else
					fallback()
				end
			end,
			s = function(fallback)
				if vim.fn["UltiSnips#CanJumpBackwards"]() == 1 then
					return vim.api.nvim_feedkeys( t("<Plug>(ultisnips_jump_backward)"), 'm', true)
				else
					fallback()
				end
			end
		}),
		['<Down>'] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }), {'i'}),
		['<Up>'] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }), {'i'}),
		['<C-n>'] = cmp.mapping({
			c = function()
				if cmp.visible() then
					cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
				else
					vim.api.nvim_feedkeys(t('<Down>'), 'n', true)
				end
			end,
			i = function(fallback)
				if cmp.visible() then
					cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
				else
					fallback()
				end
			end
		}),
		['<C-p>'] = cmp.mapping({
			c = function()
				if cmp.visible() then
					cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
				else
					vim.api.nvim_feedkeys(t('<Up>'), 'n', true)
				end
			end,
			i = function(fallback)
				if cmp.visible() then
					cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
				else
					fallback()
				end
			end
		}),
		['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), {'i', 'c'}),
		['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), {'i', 'c'}),
		['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), {'i', 'c'}),
		['<C-e>'] = cmp.mapping({ i = cmp.mapping.close(), c = cmp.mapping.close() }),
		['<CR>'] = cmp.mapping({
			i = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
		}),
	}
})

-- Use buffer source for `/`.
cmp.setup.cmdline('/', {
	completion = { autocomplete = false },
	sources = {
		{ name = 'buffer', opts = { keyword_pattern = [=[[^[:blank:]].*]=] } }
	}
})

-- Use cmdline & path source for ':'.
cmp.setup.cmdline(':', {
	sources = {
		{ name = 'path' },
		{ name = 'cmdline' }
	}
})