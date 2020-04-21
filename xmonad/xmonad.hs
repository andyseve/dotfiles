{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
-- Author: Anish Sevekari
-- Last Modified: Tue 21 Apr 2020 12:22:48 PM EDT
-- Based on : https://github.com/altercation
--
-- TODO                                                                     {{{
-------------------------------------------------------------------------------
    {-

    -}
----------------------------------------------------------------------------}}}
-- Modules                                                                  {{{
-------------------------------------------------------------------------------
-- Core
import qualified Data.Map as M
import Data.Function (on)
import Data.Maybe
import Data.List
import Data.Time.LocalTime
import System.Exit
import System.IO
-- Base
-- ||| will be imported through X.L.LayoutCombinators
import XMonad hiding ( (|||) )
import XMonad.Config.Desktop
import XMonad.StackSet ( Stack(Stack), StackSet )
import qualified XMonad.StackSet as W
-- Layout
import XMonad.Layout.Accordion
import XMonad.Layout.Decoration
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.PerScreen -- Screen Functionalities
import XMonad.Layout.PerWorkspace -- Workspace specific layouts
import XMonad.Layout.Renamed -- for modifying layout names
import XMonad.Layout.ResizableTile -- Resizable Horizontal Border
import XMonad.Layout.ShowWName
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing -- Smart space around windows
import XMonad.Layout.SubLayouts -- Layouts inside windows
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks -- Managing docks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
-- Actions
import XMonad.Actions.Commands
import XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Navigation2D
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll
-- Util
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

------------------------------------------------------------------------}}}
-- Main                                                                 {{{
---------------------------------------------------------------------------

main = do
    xmonad
        $ withNavigation2DConfig myNav2DConf
        $ ewmh
        $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
        $ myConfig


myConfig = def
    { borderWidth        = border
    , clickJustFocuses   = myClickJustFocuses
    , focusFollowsMouse  = myFocusFollowMouse
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , manageHook         = myManageHook
    , handleEventHook    = myHandleEventHook
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook
    , modMask            = myModMask
    , startupHook        = myStartupHook
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    }

----------------------------------------------------------------------------}}}
-- Workspaces                                                               {{{
-------------------------------------------------------------------------------

wsmain  = "main"
wstex   = "latex"
wscode  = "code"
wsgame  = "game"
wswww   = "www"
wscom   = "com"
wsmedia = "media"
wssys   = "sys"
wsmin   = "min"

myWorkspaces :: [String]
myWorkspaces = [ wsmain, wswww, wstex, wscode, wsgame, wscom, wsmedia, wssys, wsmin ]


myWorkspaceIcons :: String -> String
myWorkspaceIcons "main"  = "<fn=1>\xf0f2</fn>" -- 
myWorkspaceIcons "latex" = "<fn=1>\xf70e</fn>" -- 
myWorkspaceIcons "code"  = "<fn=1>\xf121</fn>" -- 
myWorkspaceIcons "game"  = "<fn=2>\xf1b6</fn>" -- 
myWorkspaceIcons "www"   = "<fn=2>\xf269</fn>" --  
myWorkspaceIcons "com"   = "<fn=1>\xf075</fn>" -- 
myWorkspaceIcons "media" = "<fn=2>\xf3b5</fn>" -- 
myWorkspaceIcons "sys"   = "<fn=1>\xf120</fn>" -- 
myWorkspaceIcons "min"   = "<fn=1>\xf328</fn>" -- 
myWorkspaceIcons _       = "<fn=1>\xf714</fn>" -- 

----------------------------------------------------------------------------}}}
-- Applications                                                             {{{
-------------------------------------------------------------------------------

myTerminal    = "alacritty"
myAltTerminal = "rxvt_unicode"
myBrowser     = "firefox"
myAltBrowser  = "google-chrome-stable"
myLauncher    = "rofi -matching fuzzy -show run -modi drun,run --disable-history -sidebar-mode -show-icon"
myAltLauncher = "dmenu_run"
myKeyViewer   = "rofi -i -dmenu -p 'Xmonad keys'"
myWinSearch   = "rofi -matching fuzzy -show window -modi window,windowcd -sidebar-mode -show-icon"
myFiles       = "alacritty -e ranger"
myEditor      = "gvim"



scratchpads = [ 
                NS "htop" spawnHtop findHtop manageHtop
              , NS "weather" spawnWeather findWeather manageWeather
              , NS "task" spawnTask findTask manageTask
              ]

    where
        spawnHtop  = "alacritty --class=scratch-htop -e htop"
        findHtop   = resource =? "scratch-htop"
        manageHtop = customFloating $ W.RationalRect x y w h
            where
                w, h, x, y :: Rational
                h = 1/2
                w = 1/2
                x = (1-w)/2
                y = (1-h)/2
        spawnWeather  = "alacritty --class=scratch-weather -e weather"
        findWeather   = resource =? "scratch-weather"
        manageWeather = customFloating $ W.RationalRect x y w h
            where
                w, h, x, y :: Rational
                h = 1/2
                w = 1/2
                x = (1-w)/2
                y = (1-h)/2
        spawnTask = "alacritty --class=scratch-task -e task"
        findTask = resource =? "scratch-task"
        manageTask = customFloating $ W.RationalRect x y w h
            where 
                w, h, x, y :: Rational
                h = 1/2
                w = 1/2
                x = (1 - w)/2
                y = (1 - h)/2
        hasName = stringProperty "WM_NAME"

----------------------------------------------------------------------------}}}
-- Theme                                                                    {{{
-------------------------------------------------------------------------------

myFocusFollowMouse = False
myClickJustFocuses = False

-- solarized colors
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"
white   = "#FFFFFF"
black   = "#000000"

-- sizes
gap    = 4
topbar = 4
border = 0
prompt = 20
status = 20

myNormalBorderColor  = base02
myFocusedBorderColor = active

active       = green
activeWarn   = red
inactive     = base02
focuscolor   = blue
unfocuscolor = base02

mySmallFont = "xft:Fira Code:style=Regular:size=6:hinting=true"
myFont      = "xft:Fira Code:style=Regular:size=8:hinting=true"
myBigFont   = "xft:Fira Code:style=Regular:size=10:hinting=true"

-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

myPromptTheme = def
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = prompt
    , position              = Top
    }

warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Top
    }

hotPromptTheme = myPromptTheme
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
    }

myShowWNameTheme = def
    { swn_font              = myBigFont
    , swn_fade              = 0.5
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
    }

----------------------------------------------------------------------------}}}
-- Layouts                                                                  {{{
-------------------------------------------------------------------------------
myLayoutHook = showWorkspaceName
                     $ fullScreenToggle
                     $ mirrorToggle
                     $ flex ||| tabs
    where
            showWorkspaceName = showWName' myShowWNameTheme

            fullScreenToggle = mkToggle (single FULL)
            mirrorToggle = mkToggle (single MIRROR)
            addTopBar = noFrillsDeco shrinkText topBarTheme

            mySpacing = spacing gap
            myGaps = gaps [(U,gap),(D,gap),(L,gap),(R,gap)]

            suffixed n = renamed [(XMonad.Layout.Renamed.AppendWords n)]
            -----------------------------------------------------------------------
            -- Tabs Layout                                                       --
            -----------------------------------------------------------------------
            tabs = named "tabs"
                     $ avoidStruts
                     $ addTopBar
                     $ addTabs shrinkText myTabTheme
                     $ Simplest
            -----------------------------------------------------------------------
            -- Flex                                                              --
            -----------------------------------------------------------------------
                    -- --------------------------------------
                    -- |                  |                 |
                    -- |                  |      Tabs       |
                    -- |                  |                 |
                    -- |      Master      | --------------- |
                    -- |                  |                 |
                    -- |                  |      Tabs       |
                    -- |                  |                 |
                    -- |                  |                 |
                    -- --------------------------------------
            flex = named "flex"
                     $ avoidStruts
                     -- Need windowNavigation to merge windows
                     $ windowNavigation
                     $ addTopBar
                     $ addTabs shrinkText myTabTheme
                     $ subLayout [] (Simplest ||| Accordion)
                     $ myGaps
                     $ mySpacing
                     $ (suffixed "1/2" $ ResizableTall 1 (1/20) (1/2) [])
                 ||| (suffixed "2/3" $ ResizableTall 1 (1/20) (2/3) [])

----------------------------------------------------------------------------}}}
-- Bindings                                                                 {{{
-------------------------------------------------------------------------------
myModMask = mod4Mask -- super key (win)
myNav2DConf = def
    { defaultTiledNavigation = centerNavigation
    , floatNavigation = centerNavigation
    , screenNavigation = lineNavigation
    , layoutNavigation = [ ("Full", centerNavigation)
                         , ("tabs", lineNavigation)
                         ]
    , unmappedWindowRect = [ ("Full", singleWindowRect)
                           , ("tabs", fullScreenRect)
                           ]
    }

-- zip commands
zipM m nm ks as f = zipWith (\k d -> (m++k, addName nm $ f d)) ks as
zipM' m nm ks as f b = zipWith (\k d -> (m++k, addName nm $ f d b)) ks as

-- workspace navigation functions -- https://github.com/altercation
-- any workspace but scratchpad
notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)

-- hidden, non-empty workspaces less scratchpad
shiftAndView' dir = findWorkspace getSortByIndexNoSP dir HiddenNonEmptyWS 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
nextHidWS = findWorkspace getSortByIndexNoSP Next HiddenWS 1
        >>= \t ->  windows . W.view $ t
prevHidWS = findWorkspace getSortByIndexNoSP Prev HiddenWS 1
        >>= \t ->  windows . W.view $ t
nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t ->  windows . W.view $ t
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t ->  windows . W.view $ t
getSortByIndexNoSP =
       fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

-- toggle any workspace but scratchpad
myToggle = windows $ W.view =<< W.tag . head . filter 
        ((\x -> x /= wsmin && x /= "SP") . W.tag) . W.hidden

-- toggling between floating and non-floating
toggleFloat w = windows (\s -> if M.member w (W.floating s)
                then W.sink w s
                else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))


wsIndices = [ 1, 5, 2, 3, 4, 6, 7, 8, 0 ]
wsKeys = map show $ wsIndices
screenKeys = ["q","w"]
dirKeys = ["j","k","h","l"]
arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
fulldirKeys = ["j", "<D>", "k", "<U>", "h", "<L>", "l", "<R>"]
fulldirs = [D,D,U,U,L,L,R,R]
dirs = [D,U,L,R]
rstrdirs = [L,R]

-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "show keybindings" $ io $ do
    h <- spawnPipe myKeyViewer
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

myKeys conf = let
    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    in
    ---------------------------------------------------------------------------
    -- System / Utilities
    ---------------------------------------------------------------------------
    subKeys "system"
    [ 
      ("M-x M-r"           , addName "restart XMonad"           $ spawn "xmonad --restart")
    , ("M-x M-S-r"         , addName "rebuild & restart XMonad" $ spawn "xmonad --recompile && xmonad --restart")
    , ("M-x M-e"           , addName "edit xmonad.hs"           $ spawn (myEditor ++ " ~/.xmonad/xmonad.hs"))
    , ("M-x M-l"           , addName "lock screen"              $ spawn "slimlock")
    , ("M1-C-l"            , addName "lock screen"              $ spawn "slimlock")
    , ("M-F1"              , addName "show keybindings"         $ return ())
    , ("M-<XF86AudioMute>" , addName "show keybindings"         $ return ())
    , ("M-x M-q"           , addName "Quit XMonad"              $ confirmPrompt hotPromptTheme "Quit XMonad"      $ io (exitWith ExitSuccess))
    ] ^++^

    ---------------------------------------------------------------------------
    -- Actions                                                               
    ---------------------------------------------------------------------------
    subKeys "actions"
    [
    -- sound
      ("M-<Page_Up>"             , addName "volume +5%"                  $ spawn "amixer set Master 5%+ unmute")
    , ("M-<Page_Down>"           , addName "volume -5%"                  $ spawn "amixer set Master 5%- unmute")
    , ("M-<End>"                 , addName "mute/unmute"                 $ spawn "amixer -q set Master toggle")
    , ("<XF86AudioRaiseVolume>"  , addName "volume +5%"                  $ spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioLowerVolume>"  , addName "volume -5%"                  $ spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioMute>"         , addName "mute/unmute"                 $ spawn "amixer set Master toggle")
    -- brightness
    , ("<XF86MonBrightnessDown>"   , addName "brightness -5"  $ spawn "light -U 5")
    , ("<XF86MonBrightnessUp>"     , addName "brightness +5"  $ spawn "light -A 5")
    , ("M-<XF86MonBrightnessDown>" , addName "brightness min" $ spawn "light -S 5")
    , ("M-<XF86MonBrightnessUp>"   , addName "brightness up"  $ spawn "light -S 100")
    -- screenshots
    , ("<Print>"                 , addName "screenshot window"           $ spawn "scrot -u \"%Y-%m-%d-%r.jpg\" -e 'mv \"$f\" ~/Pictures/screenshots/.'")
    , ("M-<Print>"               , addName "screenshot fullscreen"       $ spawn "scrot \"%Y-%m-%d-%r.jpg\" -e 'mv \"$f\" ~/Pictures/screenshots/.'")
    , ("M-C-<Print>"             , addName "screenshot region"           $ spawn "sleep 0.5; scrot -s \"%Y-%m-%d-%r.jpg\" -e 'mv \"$f\" ~/Pictures/screenshots/.'") --sleep 0.5 is to avoid keypress cancel
    ] ^++^

    ---------------------------------------------------------------------------
    -- Launchers
    ---------------------------------------------------------------------------
    subKeys "launchers"
    [
      ("M-p"          , addName "launcher"      $ spawn myLauncher)
    , ("M-S-p"        , addName "alt-launcher"  $ spawn myAltLauncher)
    , ("M-/"          , addName "window search" $ spawn myWinSearch)
    , ("M-<Return>"   , addName "terminal"      $ spawn myTerminal)
    , ("M-S-<Return>" , addName "alt-terminal"  $ spawn myAltTerminal)
    , ("M-\\"         , addName "browser"       $ spawn myBrowser)
    , ("M-s"          , addName "ssh"           $ spawn "rofi-ssh")
    , ("M-e"          , addName "files"         $ spawn myFiles)
    , ("M-z"          , addName "logout"        $ spawn "rofi-session")
    , ("M-S-o"        , addName "launcher"      $ spawn "rofi-run")
    , ("M-o M-o"      , addName "launcher"      $ spawn myLauncher)
    , ("M-o M-b"      , addName "browser"       $ spawn myBrowser)
    , ("M-o M-S-b"    , addName "alt-browser"   $ spawn myAltBrowser)
    , ("M-o M-f"      , addName "files"         $ spawn myFiles)
    , ("M-o M-t"      , addName "terminal"      $ spawn myTerminal)
    , ("M-o M-S-T"    , addName "alt-terminal"  $ spawn myAltTerminal)
    ] ^++^
    ---------------------------------------------------------------------------
    -- Windows  
    ---------------------------------------------------------------------------
    subKeys "Windows"
    (
    [
      ("M-<Backspace>" , addName "kill" kill)
    , ("M-C-<Backspace>", addName "kill all" $ confirmPrompt hotPromptTheme "kill all windows?" $ killAll)
    , ("M-m" , addName "Focus Master" $ windows W.focusMaster)
    , ("M-n" , addName "Focus Urgent" $ focusUrgent)
    , ("M-u" , addName "Tabs D" $ onGroup W.focusDown')
    , ("M-i" , addName "Tabs U" $ onGroup W.focusUp')
    , ("M-g" , addName "Unmerge" $ withFocused (sendMessage . UnMerge))
    ] 
    ++ zipM' "M-"     "navigate window"           fulldirKeys fulldirs windowGo True
    ++ zipM' "M-S-"   "move window"               fulldirKeys fulldirs windowSwap True
    ++ zipM  "M-C-"   "merge w/sublayout"         fulldirKeys fulldirs (sendMessage . pullGroup)
    ++ zipM  "M-w M-" "merge w/sublayout"         fulldirKeys fulldirs (sendMessage . pullGroup)
    ++ zipM' "M-"     "navigate screen"           screenKeys  rstrdirs screenGo True
    ++ zipM' "M-S-"   "move window to screen"     screenKeys  rstrdirs windowToScreen True
    ++ zipM' "M-C-"   "Swap workspaces to screen" screenKeys  rstrdirs screenSwap True
    ) ^++^
    ---------------------------------------------------------------------------
    -- Workspaces
    ---------------------------------------------------------------------------
    subKeys "workspaces"
    (
    [
      ("M-' M-n", addName "next non-empty workspace" $ nextHidWS)
    , ("M-' M-p", addName "prev non-empty workspace" $ prevHidWS)
    , ("M-' M-'", addName "select workspace" $ selectWorkspace myPromptTheme)
    ]
    ++ zipM "M-"     "view workspace"           wsKeys [0..] (withNthWorkspace W.greedyView)
    ++ zipM "M-S-"   "move window to workspace" wsKeys [0..] (withNthWorkspace W.shift)
    ++ zipM "M-y M-" "copy window to workspace" wsKeys [0..] (withNthWorkspace copy)
    ) ^++^
    ---------------------------------------------------------------------------
    -- Layouts and SubLayouts
    ---------------------------------------------------------------------------
    subKeys "layouts"
    [
      ("M-<Tab>",   addName "cycle all layouts"         $ sendMessage NextLayout)
    , ("M-S-<Tab>", addName "cycle sublayout"           $ toSubl NextLayout)
    , ("M-C-<Tab>", addName "reset layout"              $ setLayout $ XMonad.layoutHook conf)
    , ("M-t",       addName "toggle floating window"    $ withFocused toggleFloat)
    , ("M-S-t",     addName "tile all floating windows" $ sinkAll)
    , ("M-S-=",     addName "fullscreen"                $ sequence_ [ (withFocused $ windows . W.sink)
                                                                    , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL)
                                                                    ])
    , ("M-' M-,",   addName "Decrease master windows"  $ sendMessage (IncMasterN (-1)))
    , ("M-' M-.",   addName "Increase master windows"  $ sendMessage (IncMasterN 1))
    , ("M-' M-j",   addName "Shrink master"            $ sendMessage (Shrink))
    , ("M-' M-k",   addName "Expand master"            $ sendMessage (Expand))
    ]
----------------------------------------------------------------------------}}}
-- Startup                                                                  {{{
-------------------------------------------------------------------------------
myStartupHook = do
    spawn "~/.config/fehbg" -- feh + xrandr script
    spawnOnce "compton" -- TODO: configure settings
    spawnOnce "dunst" -- TODO: configure theme
    spawnOnce "xsetroot -cursor_name left_ptr" -- removing cross cursor
    XMonad.Hooks.DynamicBars.dynStatusBarStartup myBarCreator myBarDestroyer

quitXmonad :: X ()
quitXmonad = io (exitWith ExitSuccess)

rebuildXmonad :: X ()
rebuildXmonad = 
    spawn "xmonad --recompile && xmonad --restart"

restartXmonad :: X ()
restartXmonad = 
    spawn "xmoand --restart"

----------------------------------------------------------------------------}}}
-- Log                                                                      {{{
-------------------------------------------------------------------------------
myLogHook = do
    -- LogHook for multiple screens
    -- https://github.com/jonascj/.xmonad/blob/master/xmonad.hs 
    multiPP myLogPP myLogPP

myLogPP :: XMonad.Hooks.DynamicLog.PP
myLogPP = myXmobarLogPP

myXmobarLogPP :: XMonad.Hooks.DynamicLog.PP
myXmobarLogPP = def
    { ppCurrent = xmobarColor blue "" . myWorkspaceIcons
    , ppTitle   = xmobarColor green "" . shorten 60
    , ppVisible = xmobarColor blue "" . myWorkspaceIcons
    , ppUrgent  = xmobarColor red "" . myWorkspaceIcons
    , ppHidden  = xmobarColor white "" . myWorkspaceIcons
    , ppHiddenNoWindows = xmobarColor base01 "" . myWorkspaceIcons
    , ppSep     = " <fn=1>\xf101</fn> "
    , ppWsSep   = " "
    , ppLayout  = xmobarColor yellow ""
    , ppSort    = mkWsSort myWsCompare
    }
        where
            myWsIndex :: WorkspaceId -> Int
            myWsIndex "min" = 1000
            myWsIndex a = fixIndex $ flip elemIndex myWsOrder a
                where
                    fixIndex :: Maybe Int -> Int
                    fixIndex Nothing = 999
                    fixIndex (Just a) = a
                    myWsOrder :: [WorkspaceId]
                    myWsOrder = [wsmain, wstex, wscode, wsgame, wswww, wscom, wsmedia, wssys, wsmin]
            myWsCompare :: X WorkspaceCompare
            myWsCompare = return (compare `on` myWsIndex)

----------------------------------------------------------------------------}}}
-- Actions                                                                  {{{
-------------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = manageDocks

-------------------------------------------------------------------------------
-- New Window Actions                                                       
-------------------------------------------------------------------------------
---------------------------------------------------------------------------
-- X Event Actions
---------------------------------------------------------------------------

myHandleEventHook = docksEventHook
                -- Create a Status bar for each screen
                <+> XMonad.Hooks.DynamicBars.dynStatusBarEventHook myBarCreator myBarDestroyer

-- Defining barcreator and destroyer
myBarCreator   = myXmobarCreator
myBarDestroyer = myXmobarDestroyer

-- Xmobar Creator and Destroyer using dynamic bars
myXmobarCreator :: XMonad.Hooks.DynamicBars.DynamicStatusBar
myXmobarCreator (XMonad.S sid) = do
    t <- XMonad.liftIO Data.Time.LocalTime.getZonedTime
    XMonad.trace (show t ++ ": XMonad myXmobarCreator " ++ show sid) --logging
    XMonad.Util.Run.spawnPipe ("~/bin/xmobar/xmobar -x " ++ show sid ++ " ~/.xmonad/xmobar.hs")

myXmobarDestroyer :: XMonad.Hooks.DynamicBars.DynamicStatusBarCleanup
myXmobarDestroyer = do
    t <- XMonad.liftIO Data.Time.LocalTime.getZonedTime
    XMonad.trace (show t ++ ": XMonad myXmobarDestroyer") -- logging

----------------------------------------------------------------------------}}}

-- vim: ft=haskell:foldmethod=marker:foldlevel=4:expandtab:ts=4:sts=4:shiftwidth=4
