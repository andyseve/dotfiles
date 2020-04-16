-- http://projects.haskell.org/xmobar/
-- you can find weather location codes here: http://weather.noaa.gov/index.html

import Xmobar
import System.Environment

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
       , alpha = 20
       , position = Top
       , lowerOnStart = False
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , border = NoBorder
       , borderWidth = 0
       , commands = [  
                      Run $ Date "%a %d %b %H:%M" "date" 20
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
                    , Run $ Wireless "wlp4s0" [
                                    "-t", "<fn=1>\xf1eb</fn> <quality>" --
                                  ] 10
                    , Run $ Com ".xmonad/scripts/xmobar/weather.sh" [] "weather" 1000
                    , Run $ StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader%} %date% %weather% {%wlp4s0wi% %default:Master% "
       }

main :: IO ()
main = do
	configFromArgs config >>= xmobar

-- vim:ft=haskell
