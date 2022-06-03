-- Author: Anish Sevekari
-- Last Modified: Fri 03 Jun 2022 10:43:31 AM EDT
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
		numbers = "none", --numbers = "none" | "ordinal" | "buffer_id" | "both" | function({ ordinal, id, lower, raise }): string,
    close_command = bufdelete_helper,       -- can be a string | function, see "Mouse actions"
    right_mouse_command = "vertical sbuffer %d", -- can be a string | function, see "Mouse actions"
    left_mouse_command = "buffer %d",    -- can be a string | function, see "Mouse actions"
    middle_mouse_command = bufdelete_helper,          -- can be a string | function, see "Mouse actions"

    -- NOTE: this plugin is designed with this icon in mind,
    -- and so changing this is NOT recommended, this is intended
    -- as an escape hatch for people who cannot bear it for whatever reason
    indicator_icon = '▎',
    buffer_close_icon = '',
    modified_icon = '●',
    close_icon = '',
    left_trunc_marker = '',
    right_trunc_marker = '',

    -- name_formatter can be used to change the buffer's label in the bufferline.
    -- Please note some names can/will break the
    -- bufferline so use this at your discretion knowing that it has
    -- some limitations that will *NOT* be fixed.
    name_formatter = function(buf)  -- buf contains a "name", "path" and "bufnr"
      -- remove extension from markdown files for example
      if buf.name:match('%.md') or buf.name:match('%.txt') or buf.name:match('%.tex') then
        return vim.fn.fnamemodify(buf.name, ':t:r')
      end
    end,
    max_name_length = 18,
    max_prefix_length = 15, -- prefix used when a buffer is de-duplicated
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

    offsets = {{filetype = "NvimTree", text = "File Explorer", highlight="Directory", text_align = "left" }},
    show_buffer_icons = true, -- disable filetype icons for buffers
    show_buffer_close_icons = true,
    show_close_icon = true,
    show_tab_indicators = true,
    persist_buffer_sort = true, -- whether or not custom sorted buffers should persist
    -- can also be a table containing 2 custom separators
    -- [focused and unfocused]. eg: { '|', '|' }
		separator_style = "thick", --separator_style = "slant" | "thick" | "thin" | { 'any', 'any' },
    enforce_regular_tabs = false,
    always_show_bufferline = true,

		sort_by = 'relative_directory',
    --sort_by = 'id' | 'extension' | 'relative_directory' | 'directory' | 'tabs' | function(buffer_a, buffer_b)
      -- add custom logic
      --return buffer_a.modified > buffer_b.modified
    --end
  }
}
