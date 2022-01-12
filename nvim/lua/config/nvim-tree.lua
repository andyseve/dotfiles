-- Author: Anish Sevekari
-- Last Modified: Wed 12 Jan 2022 08:40:50 AM EST
-- # nvim-tree config

local nvim_tree = require('nvim-tree') 

vim.g.nvim_tree_show_icons = {
	git = 1,
	folders = 1,
	files = 1,
  folder_arrows = 1
}
vim.g.nvim_tree_group_empty = 1
vim.g.nvim_tree_root_folder_modifier = table.concat { ":t:gs?$?/..", string.rep(" ", 1000), "?:gs?^??" }
vim.g.nvim_tree_window_picker_exclude = {
  filetype = {
    "packer",
    "qf",
    "Trouble"
  },
	buftype = {
		'terminal'
	}
}
vim.g.nvim_tree_icons = {
	default = "",
	symlink = "",
	git = {
		deleted   = "",
		ignored   = "◌",
		renamed   = "➜",
		staged    = "✓",
		unmerged  = "",
		unstaged  = "✗",
		untracked = "★",
	},
	folder = {
		default      = "",
		open         = "",
		empty        = "",
		empty_open   = "",
		symlink      = "",
		symlink_open = "",
	},
}

vim.api.nvim_set_keymap('n', '<M-\\>', ':NvimTreeFocus<CR>', {
	noremap = true,
	silent = true
})

vim.api.nvim_set_keymap('n', '<leader>r', ':NvimTreeRefresh<CR>', {
	noremap = true,
	silent = true
})

nvim_tree.setup {
  disable_netrw       = true,
  hijack_netrw        = true,
  open_on_setup       = false,
  ignore_ft_on_setup  = {},
  auto_close          = false,
  open_on_tab         = false,
  hijack_cursor       = false,
  update_cwd          = false,
  update_to_buf_dir   = {
    enable = true,
    auto_open = true,
  },
  diagnostics = {
    enable = false,
    icons = {
      hint = "",
      info = "",
      warning = "",
      error = "",
    }
  },
  update_focused_file = {
    enable      = false,
    update_cwd  = false,
    ignore_list = {}
  },
  system_open = {
    cmd  = nil,
    args = {}
  },
  filters = {
    dotfiles = true,
    custom = {}
  },
  git = {
    enable = true,
    ignore = true,
    timeout = 500,
  },
  view = {
    width = 30,
    height = 30,
    hide_root_folder = false,
    side = 'left',
    auto_resize = false,
    mappings = {
      custom_only = false,
      list = {}
    },
    number = false,
    relativenumber = false,
    signcolumn = "yes"
  },
  trash = {
    cmd = "trash",
    require_confirm = true
  }
}
