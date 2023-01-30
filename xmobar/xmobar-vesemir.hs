Config {
	font = "xft:Fira Code Nerd Font Monospace Bold 12"
	, additionalFonts = [
		"xft:Fira Code Nerd Font Monospace Bold 10"
	]
	-- https://www.reddit.com/r/archlinux/comments/7n3uxw/font_awesome/ 
	-- use fc-list to find out correct font name
	, bgColor = "#000000"
	, fgColor = "#93a1a1"
	, alpha = 150
	, position = TopSize C 100 40
	, lowerOnStart = True
	, hideOnStart = False
	, allDesktops = True
	, persistent = True
	, border = NoBorder
	, borderWidth = 0
	, sepChar = "%"
	, alignSep = "}{"
	, template = " %UnsafeXMonadLog%}<action=xdotool key Super+c>%date%</action>{%volume% %internet% %trayer% "
	, commands = [  
			Run Date "%a %d %b %H:%M" "date" 20
		, Run PipeReader "/tmp/volume_pipe" "volume"
		, Run Com "/home/stranger/.config/xmobar/scripts/internet.sh" [] "internet" 100
		, Run Com "/home/stranger/.config/xmobar/scripts/trayer_padding_icon.sh" [] "trayer" 10
		, Run UnsafeXMonadLog
	]
}

-- vim:ft=haskell
