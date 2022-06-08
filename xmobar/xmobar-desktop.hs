import Xmobar

config :: Config
config = defaultConfig {
		font    = "xft:FiraMono Nerd Font:style=Bold:size=12:antialias=true"
	, additionalFonts = [
		"xft:FiraMono Nerd Font:style=Bold:size=10:antialias=true"
			-- https://www.reddit.com/r/archlinux/comments/7n3uxw/font_awesome/ 
			-- use fc-list to find out correct font name
		]
	, bgColor = "#000000"
	, fgColor = "#93a1a1"
	, alpha = 150
	, position = Top
	, lowerOnStart = True
	, hideOnStart = False
	, allDesktops = True
	, persistent = True
	, border = NoBorder
	, borderWidth = 0
	, commands = [  
			Run Date "%a %d %b %H:%M" "date" 20
		, Run Volume "default" "Master" [
				"--template", "<status> <fn=1><volume>%</fn>"
			, "--"
			, "-o", "\xfc5d" -- ﱝ
			, "-O", ""
			, "-h", "\xfa7d" -- 墳
			, "-m", "\xfa7f" -- 奔
			, "-l", "\xfa7e" -- 奄
			, "-c", "#dc322f"
			, "-C", "#93a1a1"
			] 10
		, Run Wireless "" [
				"-t", "\xfaa8 <fn=1><quality></fn>" --直
			] 10
		, Run DynNetwork [
				"--template" , "<dev>:<txipat>|<rxipat>"
			-- , "--Low"      , "1000"       -- units: kB/s
			-- , "--High"     , "5000"       -- units: kB/s
			] 10
		, Run Com "/home/stranger/.config/xmobar/scripts/trayer_padding_icon.sh" [] "trayer" 10
		, Run UnsafeXMonadLog
	]
	, sepChar = "%"
	, alignSep = "}{"
	, template = " %UnsafeXMonadLog%} <action=xdotool key Super+c>%date%</action> {%dynnetwork% %default:Master% %trayer%"
}

-- vim:ft=haskell
