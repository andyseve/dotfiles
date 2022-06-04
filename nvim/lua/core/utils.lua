-- Utils for nvim
-- Author: Anish Sevekari
-- Last Modified: Sat 04 Jun 2022 05:51:14 PM EDT

M = {}

-- inspect something
-- https://github.com/jamestthompson3/vimConfig/blob/eeef4a8eeb5a24938f8a0969a35f69c78643fb66/lua/tt/nvim_utils.lua#L106
M.inspect = function(item)
	print(vim.inspect(item))
end

-- check for executables
M.is_exe = function(name)
	return vim.fn.executable(name) > 0
end

vim.g["user_mappings"] = {}

M.load_mappings = function(mappings, opts)
	-- copied from nvchad: https://github.com/NvChad/NvChad/blob/main/lua/core/utils.lua
	-- can add mappings to global vim variable to make a full mappings list

	-- set mapping function with/without whichkey
	local map_func
	local present_which_key, which_key = pcall(require, "which-key")

	if present_which_key then
		map_func = function(keybind, mapping_info, opts)
			which_key.register({ [keybind] = mapping_info }, opts)
		end
	else
		map_func = function(keybind, mapping_info, opts)
			local mode = opts.mode
			opts.mode = nil
			vim.keymap.set(mode,keybind,mapping_info[0],opts)
		end
	end
	
	for _,section_mappings in pairs(mappings) do
		for mode, mode_mappings in pairs(section_mappings) do
			for keybind, mapping_info in pairs(mode_mappings) do
				local opts = vim.tbl_deep_extend("force", { mode = mode }, mapping_info.opts or {})
				local register_whichkey = mapping_info.whichkey or true
				mapping_info.opts = nil
				mapping_info.whichkey = nil

				if register_whichkey then
					map_func(keybind, mapping_info, opts)
				else
					vim.keymap.set(mode,keybind,mapping_info[0],opts)
				end
			end
		end
	end
end

M.add_mapping_groups = function(groups)
	local present_which_key, which_key = pcall(require, "which-key")
	if not present_which_key then
		return
	end

	for name,prefix in pairs(groups) do
		which_key.register({name=name},{prefix=prefix})
	end
end

return M
