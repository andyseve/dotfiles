-- Author: Anish Sevekari
-- Last Modified: Thu 02 Jun 2022 05:17:31 PM EDT
-- Plugin config file using packer

local utils = require('core.utils')
local plugins = require('core.config').plugins
local fn = vim.fn
local nvim_config = "~/.config/nvim/"

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
	vim.cmd 'packadd packer.nvim'
end

vim.cmd 'packadd packer.nvim'

return require('packer').startup({
	function()
		use {
			'wbthomason/packer.nvim',
		}

		-- ui
		use {
			'kyazdani42/nvim-web-devicons',
		}

		use {
			'akinsho/bufferline.nvim',
			disable = not plugins.bufferline,
			config = [[require('config.bufferline')]],
			requires = 'nvim-web-devicons',
		}

		use {
			'feline-nvim/feline.nvim',
			disable = not plugins.feline,
			config = [[require('config.feline')]],
			requires = 'nvim-web-devicons',
		}

		use {
			'lukas-reineke/indent-blankline.nvim',
			disable = not plugins.blankline,
			config = [[require('config.blankline')]],
			event = 'BufRead',
		}

		use {
			'folke/which-key.nvim',
			disable = not plugins.which_key,
			opt = true,
			config = [[require('config.which_key')]],
		}

		-- lsp and autocomplete
		use {
			'neoclide/coc.nvim',
			disable = not plugins.coc,
			branch = 'release',
		}

		use {
			'neovim/nvim-lspconfig', 
			disable = not plugins.lsp_config,
			config = [[require('config.lsp')]],
			requires = 'onsails/lspkind-nvim',
		}

		use {
			'ray-x/lsp_signature.nvim',
			disable = not plugins.lsp_config,
			after = 'nvim-lspconfig',
			config = [[require('config.lsp_signature')]],
		}

		use {
			'hrsh7th/nvim-cmp',
			disable = not plugins.nvim_cmp,
			config = [[require('config.cmp')]],
			requires = {
				'hrsh7th/cmp-nvim-lsp',
				'hrsh7th/cmp-path',
				'hrsh7th/cmp-omni',
				'hrsh7th/cmp-buffer',
				'f3fora/cmp-spell',
				'hrsh7th/cmp-emoji',
				'hrsh7th/cmp-cmdline',
			},
		}

		use {
			'hrsh7th/cmp-nvim-lua',
			disable = not plugins.nvim_cmp,
			after = 'nvim-cmp',
			ft = {'lua'},
		}

		use {
			'SirVer/ultisnips',
			disable = not plugins.ultisnips,
			config = [[vim.cmd('source ~/.config/nvim/config/ultisnips.vim')]],
			requires = {
				'quangnguyen30192/cmp-nvim-ultisnips'
			},
		}


		-- syntax highlight
		use {
			'norcalli/nvim-colorizer.lua',
			disable = not plugins.colorizer,
			config = [[require('colorizer').setup()]],
			event = 'BufRead',
		}

		use {
			'nvim-treesitter/nvim-treesitter',
			disable = not plugins.treesitter,
			run = ':TSUpdate',
			config = [[require('config.treesitter')]],
		}

		use {
			'itchyny/vim-highlighturl',
		}

		use {
			'numToStr/Comment.nvim',
			disable = not plugins.comment,
			config = [[require('config.comment')]],
		}

		-- themes
		use {
			'ishan9299/nvim-solarized-lua',
			disable = not plugins.solarized,
		}

		use {
			'navarasu/onedark.nvim',
			disable = not plugins.onedark,
		}

		use {
			'RRethy/nvim-base16',
			disable = not plugins.nvim_base16,
		}

		-- search and files
		use {
			'kyazdani42/nvim-tree.lua',
			disable = not plugins.nvim_tree,
			after = 'nvim-web-devicons',
			config = [[require('config.nvim-tree')]],
			setup = [[require('setup.nvim-tree')]],
			cmd = {'NvimTreeToggle', 'NvimTreeFocus'},
		}

		use {
			'nvim-telescope/telescope.nvim',
			disable = not plugins.telescope,
			config = [[require('config.telescope')]],
			setup = [[require('setup.telescope')]],
			requires = {
				'nvim-lua/plenary.nvim',
				{ 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' },
				'nvim-telescope/telescope-file-browser.nvim',
				'nvim-telescope/telescope-packer.nvim',
				'fhill2/telescope-ultisnips.nvim',
				'kelly-lin/telescope-ag',
			},
			cmd = 'Telescope',
		}

		use {
			'simnalamburt/vim-mundo',
			disable = not plugins.mundo,
			cmd = {'MundoShow', 'MundoToggle'},
		}

		use {
			'lewis6991/gitsigns.nvim',
			disable = not plugins.gitsigns,
			requires = {
				'nvim-lua/plenary.nvim'
			},
			config = [[require('config.gitsigns')]],
		}

		-- pairs
		use {
			'jiangmiao/auto-pairs',
			disable = not plugins.nvim_autopairs,
		}

		use {
			'andymass/vim-matchup',
			disable = not plugins.vim_matchup,
		}

		use {
			'tpope/vim-surround',
			disable = not plugins.vim_surround,
		}

		-- latex
		use {
			'lervag/vimtex',
			disable = not plugins.tex,
			config = [[vim.cmd('source ~/.config/nvim/config/vimtex.vim')]],
			ft = {'tex'},
		}

		-- nix
		use {
			'LnL7/vim-nix',
			disable = not plugins.nix,
			ft = {'nix'},
		}


		-- others
		use {
			'Konfekt/FastFold',
			disable = not plugins.fast_fold,
		}


		use {
			'godlygeek/tabular',
			disable = not plugins.tabular,
		}

		use {
			'Pocco81/TrueZen.nvim',
			disable = not plugins.true_zen,
			config = [[require('config.true_zen')]],
		}


	end,

	config = {
		display = {
			open_fn = function()
				return require('packer.util').float({ border='rounded' })
			end
		},
		profile = {
			enable = 1,
			threshold = 5
		}
	}
})
