-- Plugin config file using packer
-- Author: Anish Sevekari
-- Last Modified:

local utils = require("utils")
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
	vim.cmd 'packadd packer.nvim'
end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
	use { 'wbthomason/packer.nvim' }

	-- lsp and autocomplete
	--use { 'neovim/nvim-lspconfig', event = 'VimEnter', config = [[require('config.lsp')]] }
	--use { 'hrsh7th/nvim-compe', event = 'InsertEnter', config = [[require('config.compe')]] }
	--use 'SirVer/ultisnips'
	use { 'neoclide/coc.nvim', branch = 'release' }

	-- key guide
	-- use hecal3/vim-leader-guide 
	use 'spinks/vim-leader-guide'

	-- undo history
	-- use 'sjl/gundo.vim'
	use 'simnalamburt/vim-mundo'


	-- git
	use 'tpope/vim-fugitive'
	use 'junegunn/vim-github-dashboard'

	-- Latex
	use { 'lervag/vimtex', ft = {'tex'} }

	-- syntax highlight
	use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' } -- treesitter
	use { 'LnL7/vim-nix', ft = {'nix'} } -- nix
	use 'itchyny/vim-highlighturl'

	-- matching
	use 'jiangmiao/auto-pairs'
	use 'tpope/vim-surround' 
	use 'andymass/vim-matchup'

	-- notification plugin
  use 'rcarriga/nvim-notify'

	-- colorful status line and theme
	use 'vim-airline/vim-airline-themes'
	use 'vim-airline/vim-airline'

	use 'akinsho/nvim-bufferline.lua'

	-- file explorer
	-- use 'preservim/nerdtree'
	-- use 'Xuyuanp/nerdtree-git-plugin'
	use 'kyazdani42/nvim-web-devicons'
	use 'kyazdani42/nvim-tree.lua'

	-- zen mode
	use { 'folke/zen-mode.nvim', event = 'VimEnter', config = [[require('config.zen-mode')]] }

	-- themes
	use 'altercation/vim-colors-solarized'

	-- other optimizations
	use 'Konfekt/FastFold'
	use { 'tpope/vim-dispatch', cmd = { 'Dispatch', 'Make', 'Focus', 'Start' } }
	use 'tpope/vim-repeat'
	use 'scrooloose/nerdcommenter'
	use 'godlygeek/tabular'
	use 'junegunn/fzf.vim'

end)
