-- utils for nvim
-- Author: Anish Sevekari
-- Last Modified: Wed 08 Mar 2023 01:33:34 AM EST

local utils = {}

utils.termcodes = function(str)
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end

-- inspect something
-- https://github.com/jamestthompson3/vimConfig/blob/eeef4a8eeb5a24938f8a0969a35f69c78643fb66/lua/tt/nvim_utils.lua#L106
utils.inspect = function(item)
	print(vim.inspect(item))
end

-- check for executables
utils.is_exe = function(name)
	return vim.fn.executable(name) > 0
end

utils.right_align = function(left_string, right_string, fill_char)
	local width = vim.api.nvim_win_get_width(0)
	local line_number_length = vim.fn.strdisplaywidth(tostring(vim.api.nvim_buf_line_count(0)))
	if line_number_length < 4 then line_number_length = 4 end
	width = width - line_number_length
	if vim.wo.signcolumn == "yes" then width = width - 2 end
	-- utils.inspect(vim.fn.strdisplaywidth(right_string))
	local fill_width = width - vim.fn.strdisplaywidth(left_string) - vim.fn.strdisplaywidth(right_string)
	local line_fill = string.rep(fill_char, fill_width)
	return left_string .. line_fill .. right_string
end

-- defining dictionary to hold user mappings
-- this is a global variable
_User_Mappings = {}
_User_Prefixes = {}


utils.load_mappings = function(mappings, opts)
	-- copied from nvchad: https://github.com/NvChad/NvChad/blob/main/lua/core/utils.lua
	-- set mapping function with/without whichkey

	local present_whichkey, whichkey = pcall(require, "which-key")

	for name, section in pairs(mappings) do
		-- section.whichkey determines if sections is to be registered or not
		-- section.opts sets default options for the section
		-- section.name specifies which-key name
		-- section.prefix determines prefix of the section
		local section_opts = vim.deepcopy(section.opts or {})
		local section_name = section.name or name
		local section_prefix = section.prefix or nil
		local section_whichkey = true
		if section.whichkey ~= nil then section_whichkey = section.whichkey end
		if section_prefix == nil then section.whichkey = false end

		-- If prefix already has a group name, then keep the original name, and notify the user
		if section_prefix ~= nil then
			if _User_Prefixes[section_prefix] == nil then
				_User_Prefixes[section_prefix] = section_name
			elseif _User_Prefixes[section_prefix] ~= section_name then
				utils.inspect("Conficting names: " .. _User_Prefixes[section_prefix] .. " and " .. section_name)
				section_name = _User_Prefixes[section_prefix]
			end
		end

		-- remove these options in order to iterate over the rest
		section.opts = nil
		section.whichkey = nil
		section.name = nil
		section.prefix = nil


		for mode, mode_mappings in pairs(section) do
			for keybind, mapping_info in pairs(mode_mappings) do
				local mapping_opts = vim.tbl_deep_extend("force",
					opts or {},
					section_opts or {},
					mapping_info.opts or {}
				)
				local mapping_desc = mapping_info.name or mapping_info[2] or nil
				local mapping_keybind = (section_prefix or "") .. keybind
				mapping_info.opts = nil
				mapping_info.whichkey = nil

				mapping_opts.desc = mapping_desc

				-- leverage whichkey's ability to read desc property of mappings
				vim.keymap.set(mode, mapping_keybind, mapping_info[1], mapping_opts)

				table.insert(_User_Mappings, {
					name = section_name .. "  " .. mapping_info[2],
					section = section_name,
					keybind = mapping_keybind,
					cmd = mapping_info[1],
					opts = mapping_opts
				})
			end
		end

		if present_whichkey and section_whichkey then
			whichkey.register({ [section_prefix] = section_name })
		end

	end
end

-- Clear logfile and Insert date on top.
local logfile = vim.fn.stdpath("cache") .. "/userconfig.log"
local logfile_w = assert(io.open(logfile, "w"))
io.output(logfile_w)
io.write(os.date() .. "\n")
io.close(logfile_w)

-- log into a file
utils.log = function(item)
	local logfile_io = assert(io.open(logfile, "a"))
	io.output(logfile_io)
	io.write(vim.inspect(item) .. "\n")
	io.close(logfile_io)
end

return utils
