Config {
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
		"-t", "<status> <fn=1><volume>%</fn>"
			, "--"
			, "-o", "\xfc5d" -- ﱝ
			, "-O", ""
			, "-h", "\xfa7d" -- 墳
			, "-m", "\xfa7f" -- 奔
			, "-l", "\xfa7e" -- 奄
			, "-c", "#dc322f"
			, "-C", "#93a1a1"
		] 10
		, Run Network "enp2s0f1" [
		"-t", "\xfaa8 <fn=1><quality></fn>" --直
		] 10
		, Run UnsafeXMonadLog
		]
		, sepChar = "%"
		, alignSep = "}{"
		, template = " %UnsafeXMonadLog%} <action=xdotool key Super+c>%date%</action> {%enp2s0f1% %default:Master% "
}

-- vim:ft=haskell
