-- http://projects.haskell.org/xmobar/
-- Last Modified: Wed 29 Sep 2021 05:05:33 PM EDT
-- Author: Anish Sevekari

import Xmobar
import System.Environment

config :: Config
config = defaultConfig { 
				 font    = "xft:Fira Code:style=Bold:size=12:antialias=true"
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
										  Run $ Date "%a %d %b %H:%M" "date" 20
                    , Run $ Volume "default" "Master" [
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
                    , Run $ Wireless "wlp4s0" [
                                    "-t", "\xfaa8 <fn=1><quality></fn>" --直
                                  ] 10
										, Run $ Com ".xmonad/scripts/xmobar/weather.sh" [] "weather" 1000
										,	Run $ Battery [
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
											 , "--lows"              , "\xf244" --
											 , "--mediums"           , "\xf243" --
											 , "--highs"             , "\xf242" --
											 , "--on-icon-pattern"   , "\xf0e7" --
											 , "--idle-icon-pattern" , "\xf0e9" --
											 , "-A" , "5"
											 , "-a" , "notify-send -u critical --hint=string:x-dunst-stack-tag:low_battery -a Battery -i /run/current-system/sw/share/icons/Papirus-Dark/48x48/status/battery-empty.svg \"Battery Low\" \"Your computer will turn of soon\""
											 ] 50
										, Run $ Brightness [
													   "-t", "\xf185 <fn=1><percent>%</fn>" --
													 , "--"
													 , "-D", "intel_backlight"
													 ] 10
										, Run $ UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
			 , template = " %UnsafeStdinReader%} <action=xdotool key Super+c>%date%</action> %weather% {%wlp1s0wi% %default:Master% %bright% %battery%"
       }

main :: IO ()
main = do
	configFromArgs config >>= xmobar

-- vim:ft=haskell
