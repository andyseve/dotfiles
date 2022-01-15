-- Utils for nvim
-- Author: Anish Sevekari
-- Last Modified: Sat Jan 15 16:50:59 2022

-- inspect something
-- https://github.com/jamestthompson3/vimConfig/blob/eeef4a8eeb5a24938f8a0969a35f69c78643fb66/lua/tt/nvim_utils.lua#L106
function inspect(item)
	print(vim.inspect(item))
end

-- check for executables
function is_exe(name)
	return vim.fn.executable(name) > 0
end

-- delete buffer without losing layout
-- the github link for code
