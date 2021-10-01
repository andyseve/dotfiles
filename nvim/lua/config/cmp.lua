-- Author: Anish Sevekari
-- Last Modified: Fri 01 Oct 2021 01:32:05 AM EDT
-- # nvim-cmp settings

local cmp = require('cmp')
local lspkind = require('lspkind')

local t = function(str)
	return vim.api.nvim_replace_termcodes(str,true,true,true)
end

cmp.setup({
	snippet = {
		expand = function(args)
			vim.fn["UltiSnips#Anon"](args.body)
		end,
	},

	sources = {
		{ name = 'ultisnips' },
		{ name = 'nvim_lsp' },
		{ name = 'buffer' },
		{ name = 'path' },
		{ name = 'spell' },
		{ name = 'emoji' }
	},

	mapping = {
		["<c-space>"] = cmp.mapping(function(fallback)
			if vim.fn.pumvisible() == 1 then
				if vim.fn["UltiSnips#CanExpandSnippet"]() == 1 then
					return vim.fn.feedkeys(t("<c-r>=UltiSnips#ExpandSnippet()<cr>"))
				end
				vim.fn.feedkeys(t("<c-n>"), "n")
			else
				fallback()
			end
		end, {
			"i", 
			"s",
		}),
		["<tab>"] = cmp.mapping(function(fallback)
			if vim.fn.complete_info()["selected"] == -1 and vim.fn["UltiSnips#CanExpandSnippet"]() == 1 then
				vim.fn.feedkeys(t("<c-r>=UltiSnips#ExpandSnippet()<cr>"))
			elseif vim.fn["UltiSnips#CanJumpForwards"]() == 1 then
				vim.fn.feedkeys(t("<esc>:call UltiSnips#JumpForwards()<cr>"))
			elseif vim.fn.pumvisible() == 1 then
				vim.fn.feedkeys(t("<c-n>"), "n")
			else
				fallback()
			end
		end, {
			"i",
			"s",
		}),
		["<s-tab>"] = cmp.mapping(function(fallback)
			if vim.fn["UltiSnips#CanJumpBackwards"]() == 1 then
				return vim.fn.feedkeys(t("<c-r>=UltiSnips#JumpBackwards()<cr>"))
			elseif vim.fn.pumvisible() == 1 then
				vim.fn.feedkeys(t("<c-p>"), "n")
			else
				fallback()
			end
		end, {
			"i",
			"s",
		}),
		["<cr>"] = cmp.mapping.confirm(),
	},

	completion = {
		keyword_length = 3
	},

	formatting = {
		format = function(_, vim_item)
			vim_item.kind = lspkind.presets.default[vim_item.kind] .. ' ' .. vim_item.kind
			return vim_item
		end,
	},
})

