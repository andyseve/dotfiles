Config {
	font = "xft:Fira Code Nerd Font Mono Bold 12"
	, additionalFonts = [
		"xft:Fira Code Nerd Font Mono Bold 10"
	]
	-- https://www.reddit.com/r/archlinux/comments/7n3uxw/font_awesome/ 
	-- use fc-list to find out correct font name
	, border = NoBorder
	, borderWidth = 0
	, bgColor = "#000000"
	, fgColor = "#93a1a1"
	, alpha = 150
	, position = TopSize C 100 40
	, allDesktops = True
	, persistent = False
	, lowerOnStart = True
	, hideOnStart = False
	, sepChar = "%"
	, commands = [  
			Run Date "%a %d %b %H:%M" "date" 20
		, Run PipeReader "/tmp/volume_pipe" "volume"
		, Run Com "/home/stranger/.config/xmobar/scripts/internet.sh" [] "internet" 100
		, Run PipeReader "/tmp/music_pipe" "music"
		, Run UnsafeXMonadLog
	]
	, alignSep = "}{"
	, template = "\
	\  \
	\%UnsafeXMonadLog%\
	\}\
	\<action=xdotool key Super+c>%date%</action>\
	\{\
	\%music% \
	\%volume% \
	\"
}

-- vim:ft=haskell
