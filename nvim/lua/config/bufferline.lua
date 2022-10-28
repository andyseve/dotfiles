-- Author: Anish Sevekari
-- Last Modified: Fri 28 Oct 2022 05:53:49 AM EDT
-- bufferline settings

local present, bufferline = pcall(require, 'bufferline')

if not present then
	return
end

local present_bufdelete, bufdelete = pcall(require, 'bufdelete')
local function bufdelete_helper(bufnum)
	if present_bufdelete then
		bufdelete.bufdelete(bufnum, true)
	else
		vim.cmd('bdelete!', bufnum)
	end
end

bufferline.setup {
  options = {
    mode = "buffers", -- set to "tabs" to only show tabpages instead
		numbers = "none", --numbers = "none" | "ordinal" | "buffer_id" | "both" | function({ ordinal, id, lower, raise }): string,
    close_command = bufdelete_helper,       -- can be a string | function, see "Mouse actions"
    right_mouse_command = "vertical sbuffer %d", -- can be a string | function, see "Mouse actions"
    left_mouse_command = "buffer %d",    -- can be a string | function, see "Mouse actions"
    middle_mouse_command = bufdelete_helper,          -- can be a string | function, see "Mouse actions"

    -- NOTE: this plugin is designed with this icon in mind,
    -- and so changing this is NOT recommended, this is intended
    -- as an escape hatch for people who cannot bear it for whatever reason
		indicator = {
			icon = '▎',
			style = 'icon',		
		},
    buffer_close_icon = '',
    modified_icon = '●',
    close_icon = '',
    left_trunc_marker = '',
    right_trunc_marker = '',

    -- name_formatter can be used to change the buffer's label in the bufferline.
    -- Please note some names can/will break the
    -- bufferline so use this at your discretion knowing that it has
    -- some limitations that will *NOT* be fixed.
    name_formatter = function(buf)  -- buf contains:
			-- name                | str        | the basename of the active file
			-- path                | str        | the full path of the active file
			-- bufnr (buffer only) | int        | the number of the active buffer
			-- buffers (tabs only) | table(int) | the numbers of the buffers in the tab
			-- tabnr (tabs only)   | int        | the "handle" of the tab, can be converted to its ordinal number using: `vim.api.nvim_tabpage_get_number(buf.tabnr)`

      -- remove extension from markdown files for example
      if buf.name:match('%.md') or buf.name:match('%.txt') or buf.name:match('%.tex') then
        return vim.fn.fnamemodify(buf.name, ':t:r')
      end
    end,
    max_name_length = 18,
    max_prefix_length = 15, -- prefix used when a buffer is de-duplicated
    truncate_names = true -- whether or not tab names should be truncated
    tab_size = 18,
    diagnostics = "nvim_lsp",	--diagnostics = false | "nvim_lsp" | "coc",
    diagnostics_update_in_insert = false,
    diagnostics_indicator = function(count, level, diagnostics_dict, context)
      return "("..count..")"
    end,

    -- NOTE: this will be called a lot so don't do any heavy processing here
    custom_filter = function(buf_number)
			buf_filetype = vim.bo[buf_number].filetype
      -- filter out filetypes you don't want to see
      if buf_filetype ~= "NvimTree" and buf_filetype ~= "qf" then
        return true
      end
    end,
		offsets = {
			{
				filetype = "NvimTree",
				text = "File Explorer" -- text = "File Explorer" | function ,
				text_align = "left" -- text_align = "left" | "center" | "right"
				separator = true
			}
		},
		color_icons = true,
    show_buffer_icons = true, -- disable filetype icons for buffers
    show_buffer_close_icons = true,
    show_buffer_default_icon = true, -- whether or not an unrecognised filetype should show a default icon
    show_close_icon = true,
    show_tab_indicators = true,
    show_duplicate_prefix = true, -- whether to show duplicate buffer prefix
    persist_buffer_sort = true, -- whether or not custom sorted buffers should persist
    -- can also be a table containing 2 custom separators
    -- [focused and unfocused]. eg: { '|', '|' }
		separator_style = "thick", --separator_style = "slant" | "thick" | "thin" | { 'any', 'any' },
    enforce_regular_tabs = false,
    always_show_bufferline = true,

		-- hover = {
		-- 	enabled = true,
		-- 	delay = 200,
		-- 	reveal = {'close'}
		-- },

		sort_by = 'relative_directory',
    --sort_by = 'id' | 'extension' | 'relative_directory' | 'directory' | 'tabs' | function(buffer_a, buffer_b)
      -- add custom logic
      --return buffer_a.modified > buffer_b.modified
    --end
  }
}
