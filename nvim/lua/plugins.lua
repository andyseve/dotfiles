-- Author: Anish Sevekari
-- Last Modified: Thu 17 Feb 2022 03:42:50 AM EST
-- Plugin config file using packer

local utils = require('core.utils')
local plugins = require('core.config').plugins
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
	vim.cmd 'packadd packer.nvim'
end

vim.cmd 'packadd packer.nvim'

return require('packer').startup({
	function()
		use {
			'wbthomason/packer.nvim'
		}

		-- ui
		use {
			'kyazdani42/nvim-web-devicons'
		}

		use {
			'akinsho/nvim-bufferline.lua',
			after = 'nvim-web-devicons',
			config = [[require('config.bufferline')]]
		}

		use {
			'feline-nvim/feline.nvim',
			after = 'nvim-web-devicons',
			config = [[require('config.feline')]]
		}

		use {
			'lukas-reineke/indent-blankline.nvim',
			disable = not plugins.blankline,
			config = [[require('config.blankline')]],
			event = 'BufRead'
		}

		--use 'vim-airline/vim-airline-themes'
		--use 'vim-airline/vim-airline'
		-- use hecal3/vim-leader-guide 
		-- use 'spinks/vim-leader-guide'

		-- lsp and autocomplete
		use {
			'neoclide/coc.nvim',
			disable = not plugins.coc,
			branch = 'release'
		}

		use {
			'neovim/nvim-lspconfig', 
			disable = not plugins.lsp_config,
			config = [[require('config.lsp')]],
			requires = 'onsails/lspkind-nvim'
		}

		use {
			'ray-x/lsp_signature.nvim',
			disable = not plugins.lsp_config,
			after = 'nvim-lspconfig',
			config = [[require('config.lsp_signature')]]
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
			}
		}

		use {
			'hrsh7th/cmp-nvim-lua',
			disable = not plugins.nvim_cmp,
			after = 'nvim-cmp',
			ft = {'lua'}
		}

		use {
			'SirVer/ultisnips',
			disable = not plugins.ultisnips,
			requires = {
				'quangnguyen30192/cmp-nvim-ultisnips'
			}
		}


		-- syntax highlight
		use {
			'norcalli/nvim-colorizer.lua',
			disable = not plugins.colorizer,
			event = 'BufRead',
		}

		use {
			'nvim-treesitter/nvim-treesitter',
			disable = not plugins.treesitter,
			run = ':TSUpdate',
			config = [[require('config.treesitter')]]
		}

		use {
			'itchyny/vim-highlighturl'
		}

		use {
			'numToStr/Comment.nvim',
			disable = not plugins.comment,
			config = [[require('config.comment')]]
		}

		use {
			'scrooloose/nerdcommenter',
			disable = not plugins.nerdcommenter
		}

		-- themes
		use {
			'ishan9299/nvim-solarized-lua',
			disable = not plugins.solarized
		}

		use {
			'navarasu/onedark.nvim',
			disable = not plugins.onedark
		}

		-- search and files
		use {
			'kyazdani42/nvim-tree.lua',
			disable = not plugins.nvim_tree,
			after = 'nvim-web-devicons',
			cmd = {'NvimTreeToggle', 'NvimTreeFocus'},
			config = [[require('config.nvim-tree')]],
			setup = [[require('setup.nvim-tree')]]
		}

		use {
			'preservim/nerdtree',
			disable = not plugins.nerdtree
		}

		use {
			'Xuyuanp/nerdtree-git-plugin',
			disable = not plugins.nerdtree
		}

		use {
			'junegunn/fzf.vim',
			disable = not plugins.fzf
		}

		use {
			'nvim-telescope/telescope.nvim',
			disable = not plugins.telescope,
			cmd = 'Telescope',
			setup = [[require('setup.telescope')]],
			requires = {
				'nvim-lua/plenary.nvim'
			}
		}

		use {
			'simnalamburt/vim-mundo',
			disable = not plugins.mundo,
			cmd = {'MundoShow', 'MundoToggle'}
		}

		use {
			'lewis6991/gitsigns.nvim',
			disable = not plugins.gitsigns,
			requires = {
				'nvim-lua/plenary.nvim'
			},
			config = [[require('config.gitsigns')]]
		}

		-- pairs
		use {
			'jiangmiao/auto-pairs'
		}

		use {
			'andymass/vim-matchup',
			disable = not plugins.vim_matchup
		}

		use {
			'tpope/vim-surround',
			disable = not plugins.vim_surround
		}

		-- latex
		use {
			'lervag/vimtex',
			disable = not plugins.tex,
			ft = {'tex'}
		}

		-- nix
		use {
			'LnL7/vim-nix',
			disable = not plugins.nix,
			ft = {'nix'}
		}


		-- others
		use {
			'Konfekt/FastFold',
			disable = not plugins.fast_fold
		}


		use {
			'godlygeek/tabular',
			disable = not plugins.tabular
		}

		use {
			'Pocco81/TrueZen.nvim',
			disable = not plugins.true_zen,
			config = [[require('config.true_zen')]]
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
