-- http://projects.haskell.org/xmobar/
-- you can find weather location codes here: http://weather.noaa.gov/index.html

import Xmobar

config :: Config
config = defaultConfig { 
				 font    = "xft:Fira Code:style=Bold:size=10:antialias=true"
			 , additionalFonts = [
													"xft:FontAwesome5Free:style=Solid:size=10",
													"xft:FontAwesome5Brands:style=Solid:size=10"
													-- https://www.reddit.com/r/archlinux/comments/7n3uxw/font_awesome/ 
												]
			 , iconRoot= ".xmonad/icons"
       , bgColor = "#002b36"
       , fgColor = "#93a1a1"
       , alpha = 0
       , position = Top
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , border = NoBorder
       , borderWidth = 0
       , commands = [  
											Run $ Battery [
											   "--template" , "<acstatus> (<timeleft>)"
											 , "--Low"      , "25"
											 , "--High"     , "50"
											 , "--low"      , "#dc322f"
											 , "--high"     , "#859900"
											 , "--" --battery specific options
														-- discharging
											 , "-o" , "<leftipat> <left>%"
														-- AC
											 , "-O" , "<leftipat> <left>%"
											 , "-i" , "<leftipat> <left>%"
											 , "--off-icon-pattern"  , ""
											 , "--lows"              , "<fn=1>\xf244</fn>" --
											 , "--mediums"           , "<fn=1>\xf243</fn>" --
											 , "--highs"             , "<fn=1>\xf242</fn>" --
											 , "--on-icon-pattern"   , "<fn=1>\xf0e7</fn>" --
											 , "--idle-icon-pattern" , "<fn=1>\xf0e9</fn>" --
											 , "-A" , "5"
											 , "-a" , "systemctl hibernate"
											 ] 50
										, Run $ Date "%a %d %b %H:%M" "date" 20
										, Run $ Volume "default" "Master" [
																						"-t", "<status> <volume>%"
																					, "--"
																					, "-o", "<fn=1>\xf6a9</fn>" --
																					, "-O", ""
																					, "-h", "<fn=1>\xf028</fn>" --
																					, "-m", "<fn=1>\xf027</fn>" --
																					, "-l", "<fn=1>\xf026</fn>" --
																					, "-c", "#dc322f"
																					, "-C", "#93a1a1"
																					] 10
										, Run $ Brightness [
													   "-t", "<fn=1>\xf185</fn> <percent>%" --
													 , "--"
													 , "-D", "intel_backlight"
													 ] 10
										, Run $ Wireless "wlp1s0" [
																		"-t", "<fn=1>\xf1eb</fn> <quality>" --
																	] 10
										, Run $ Com ".xmonad/scripts/xmobar/weather.sh" [] "weather" 1000
										, Run $ StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
			 , template = " %StdinReader%} %date% %weather% {%wlp1s0wi% %default:Master% %bright% %battery%"
       }

main :: IO ()
main = xmobar config

-- vim:ft=haskell
