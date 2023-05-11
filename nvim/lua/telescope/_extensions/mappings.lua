local finders = require("telescope.finders")
local pickers = require("telescope.pickers")
local conf = require("telescope.config").values
local entry_display = require("telescope.pickers.entry_display")

local print_keybinds = function(keybind)
	local str = string.gsub(keybind, "(.)", "%1 ")
	str = string.gsub(str, "backspace", "BSP")
	local replacements = {
		["backspace"] = "BSP",
		["leader"] = "LDR",
		["escape"] = "ESC",
		["space"] = "SPC",
	}
	for s, r in pairs(replacements) do
		local pat = string.gsub(s, "(.)", "%1 ")
		str = string.gsub(str, pat, r)
	end
	for _, r in pairs(replacements) do
		local pat = string.gsub(r, "(.)", "%1 ")
		str = string.gsub(str, pat, r)
	end
	for _, r in pairs(replacements) do
		local pat = "<%s*" .. r .. "%s*>"
		str = string.gsub(str, pat, r)
	end
	str = string.gsub(str, "<%s*(%a+)%s*-%s*(%S+)%s*>", function(a, b) return string.upper(a) .. "+" .. b end)
	return str
end

local action = function(keymap)
	print(keymap.description)
end

local search = function(opts)
	local displayer = entry_display.create({
		separator = " ",
		items = {
			{ width = 20 },
			{ width = 40 },
			{ remaining = true },
		},
	})
	local make_display = function(entry)
		return displayer({
			print_keybinds(entry.keybind),
			entry.name,
			tostring(entry.cmd)
		})
	end

	pickers.new(opts, {
		prompt_title = "Key Maps",
		sorter = conf.generic_sorter(opts),
		finder = finders.new_table({
			results = _User_Mappings,
			entry_maker = function(keymap)
				return {
					ordinal = keymap.name .. keymap.keybind .. tostring(keymap.cmd),
					display = make_display,

					name = keymap.name,
					keybind = keymap.keybind,
					cmd = tostring(keymap.cmd),
					section = keymap.section
				}
			end,
		}),
	}):find()
end

return require("telescope").register_extension({
	setup = function(config)
		action = config.action or action
	end,
	exports = { mappings = search },
})
