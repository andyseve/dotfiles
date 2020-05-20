{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
-- Author: Anish Sevekari
-- Last Modified: Wed 20 May 2020 04:37:51 PM EDT
-- Based on : https://github.com/altercation
--
-- TODO                                                                     {{{
-------------------------------------------------------------------------------
    {-
    * Add urgency hook
    * scratchpads
    * restructure xmobar
    * xmobar music
    * xmobar weather
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
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.PerScreen -- Screen Functionalities
import XMonad.Layout.PerWorkspace -- Workspace specific layouts
import XMonad.Layout.PositionStoreFloat -- Position remebering floats
import XMonad.Layout.Renamed -- for modifying layout names
import XMonad.Layout.ResizableTile -- Resizable Horizontal Border
import XMonad.Layout.ShowWName
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing -- Smart space around windows
import XMonad.Layout.SubLayouts -- Layouts inside windows
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks -- Managing docks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.PositionStoreHooks
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
import XMonad.Util.Image
import XMonad.Util.NamedActions as NA
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

------------------------------------------------------------------------}}}
-- Main                                                                     {{{
-------------------------------------------------------------------------------

main = do
    xmonad
         $ withNavigation2DConfig myNav2DConf
         $ withUrgencyHook LibNotifyUrgencyHook
         $ ewmh
         $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
         $ myConfig


myConfig = def
    { borderWidth        = baseBorder
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
wsmin   = "NSP"

myWorkspaces :: [String]
myWorkspaces = [ wsmain, wswww, wstex, wscode, wsgame, wscom, wsmedia, wssys, wsmin ]



----------------------------------------------------------------------------}}}
-- Applications                                                             {{{
-------------------------------------------------------------------------------

myTerminal    = "alacritty"
myAltTerminal = "rxvt_unicode"
myBrowser     = "firefox"
myAltBrowser  = "google-chrome-stable"
myLauncher    = "rofi -show drun -modi drun,run"
myAltLauncher = "rofi -show run -modi drun,run"
myKeyViewer   = "rofi -i -dmenu -p 'Xmonad keys'"
myWinSearch   = "rofi -show window -modi window,windowcd"
myFiles       = "alacritty -e ranger"
myEditor      = "gvim"
myMusic       = "google-play-music-desktop-player"

----------------------------------------------------------------------------}}}
-- Scratchpads                                                              {{{
-------------------------------------------------------------------------------

myScratchpads = [ NS "htop"  spawnHtop findHtop manageHtop
                , NS "task"  spawnTask findTask manageTask
                , NS "mixer" spawnMixer findMixer managerMixer
                , NS "ranger" spawnRanger findRanger manageRanger
                ]

    where
        centerFloating = customFloating $ W.RationalRect x y w h
            where
                w, h, x, y :: Rational
                h = 0.5
                w = 0.5
                x = 0.25
                y = 0.25
        rightFloating = customFloating $ W.RationalRect x y w h
            where
                w, h, x, y :: Rational
                h = 0.25
                w = 0.25
                x = 0.73
                y = 0.02

        spawnHtop  = myTerminal ++ " --class=htop -e htop"
        findHtop   = resource =? "htop"
        manageHtop = centerFloating

        spawnWeather  = myTerminal ++ "--class=weather -e weather"
        findWeather   = resource =? "weather"
        manageWeather = centerFloating

        spawnTask = myTerminal ++ " --class=task -e tasksh"
        findTask = resource =? "task"
        manageTask = centerFloating

        spawnMixer = myTerminal ++ " --class=volume -e alsamixer"
        findMixer = resource =? "volume" <||> title=? "alsamixer"
        managerMixer = rightFloating

        spawnRanger = myTerminal ++ " --class=ranger  -e ranger"
        findRanger = resource =? "ranger"
        manageRanger = centerFloating

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



myNormalBorderColor  = base02
myFocusedBorderColor = active

active       = green
activeWarn   = red
inactive     = base02
focuscolor   = blue
unfocuscolor = base02

-- fonts

mySmallFont = "xft:Fira Code:style=Regular:size=6:hinting=true"
myFont      = "xft:Fira Code:style=Regular:size=8:hinting=true"
myBigFont   = "xft:Fira Code:style=Regular:size=10:hinting=true"

-- sizes
gap    = 4
topbar = 4
toptitle = 22
baseBorder = 0
prompt = 20
status = 20
myBorder = Border{ top = gap, bottom = gap , right = gap , left = gap }

-- this is a "fake title" used as a highlight bar in lieu of full borders
myTopBarTheme :: Theme
myTopBarTheme = def
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

myTabTheme :: Theme
myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base03
    , urgentColor           = red
    , activeBorderColor     = base03
    , inactiveBorderColor   = base1
    , urgentBorderColor     = base1
    , activeTextColor       = base03
    , inactiveTextColor     = base1
    , urgentTextColor       = base1
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

myButtonTheme :: Theme
myButtonTheme = defaultThemeWithImageButtons
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base03
    , urgentColor           = red
    , activeBorderColor     = base03
    , inactiveBorderColor   = base1
    , urgentBorderColor     = base1
    , activeTextColor       = base03
    , inactiveTextColor     = base1
    , urgentTextColor       = base1
    , decoHeight            = toptitle
    , windowTitleIcons      = [ (menuButton , CenterLeft   3)
                              , (closeButton, CenterRight  3)
                              , (maxiButton , CenterRight 18)
                              , (miniButton , CenterRight 33)
                              ]
    }
    where  -- Button definitions                                          --{{{
        menuButton'  :: [[Int]]
        menuButton'  =  [[0,0,0,0,0,0,0,0,0,0],
                         [0,1,0,0,0,0,0,0,1,0],
                         [0,0,1,1,0,0,1,1,0,0],
                         [0,0,1,0,0,0,0,1,0,0],
                         [0,0,0,0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0,0,0,0],
                         [0,0,1,0,0,0,0,1,0,0],
                         [0,0,1,1,0,0,1,1,0,0],
                         [0,1,0,0,0,0,0,0,1,0],
                         [0,0,0,0,0,0,0,0,0,0]] 
        miniButton'  :: [[Int]]
        miniButton'  =  [[0,0,0,0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0,0,0,0],
                         [0,1,1,1,1,1,1,1,1,0],
                         [0,1,1,1,1,1,1,1,1,0],
                         [0,0,0,0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0,0,0,0]]
        maxiButton'  :: [[Int]]
        maxiButton'  =  [[0,0,0,0,0,0,0,0,0,0],
                         [0,1,1,1,1,1,1,1,1,0],
                         [0,1,1,1,1,1,1,1,1,0],
                         [0,1,1,0,0,0,0,1,1,0],
                         [0,1,1,0,0,0,0,1,1,0],
                         [0,1,1,0,0,0,0,1,1,0],
                         [0,1,1,0,0,0,0,1,1,0],
                         [0,1,1,1,1,1,1,1,1,0],
                         [0,1,1,1,1,1,1,1,1,0],
                         [0,0,0,0,0,0,0,0,0,0]]
        closeButton' :: [[Int]] 
        closeButton' =  [[0,0,0,0,0,0,0,0,0,0],
                         [0,1,1,0,0,0,0,1,1,0],
                         [0,1,1,1,0,0,1,1,1,0],
                         [0,0,1,1,1,1,1,1,0,0],
                         [0,0,0,1,1,1,1,0,0,0],
                         [0,0,0,1,1,1,1,0,0,0],
                         [0,0,1,1,1,1,1,1,0,0],
                         [0,1,1,1,0,0,1,1,1,0],
                         [0,1,1,0,0,0,0,1,1,0],
                         [0,0,0,0,0,0,0,0,0,0]]
        convertToBool :: [Int] -> [Bool]
        convertToBool = map (\x -> x == 1)
        menuButton  = map convertToBool menuButton'
        miniButton  = map convertToBool miniButton'
        maxiButton  = map convertToBool maxiButton'
        closeButton = map convertToBool closeButton'
        --------------------------------------------------------------------}}}

----------------------------------------------------------------------------}}}
-- Layouts                                                                  {{{
-------------------------------------------------------------------------------
myLayoutHook = showWorkspaceName
    $ fullScreenToggle
    $ onWorkspace wsgame ( tabs ||| float )
    $ onWorkspace wsmedia ( tabs ||| float )
    $ onWorkspace wscom ( tabs ||| float )
    $ ( tall ||| tripane )
    where
        showWorkspaceName = showWName' myShowWNameTheme

        fullScreenToggle = mkToggle (single FULL)
        addTopBar = noFrillsDeco shrinkText myTopBarTheme

        mySpacing = spacingRaw True myBorder False myBorder True

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
        -- Two Columns Layout                                                --
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
        tall = named "tall"
            $ avoidStruts
            -- Need windowNavigation to merge windows
            $ windowNavigation
            $ addTopBar
            $ addTabs shrinkText myTabTheme
            $ mySpacing
            $ subLayout [] (Simplest ||| Accordion)
            $ (suffixed "1/2" $ ResizableTall 1 (1/20) (1/2) [2, 1])
            ||| (suffixed "2/3" $ ResizableTall 1 (1/20) (3/5) [])

        -----------------------------------------------------------------------
        -- Three Columns Layout                                              --
        -----------------------------------------------------------------------
            -- 3 Columns: master on left
            -- 3 Columns master in middle
        tripane = named "tri"
            $ avoidStruts
            $ windowNavigation
            $ addTopBar
            $ addTabs shrinkText myTabTheme
            $ mySpacing
            $ subLayout [] (Simplest ||| Accordion)
            $ (suffixed "mid" $ ThreeColMid 1 (3/100) (1/3))
            ||| (suffixed "left" $ ThreeCol 1 (3/100) (1/2))

        -----------------------------------------------------------------------
        -- Three Columns Layout
        -----------------------------------------------------------------------
        float = named "float"
            $ avoidStruts
            $ imageButtonDeco shrinkText myButtonTheme 
            $ positionStoreFloat

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

                
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "show keybindings" $ io $ do
    h <- spawnPipe myKeyViewer
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

myKeys conf = let
    subKeys str ks = subtitle str : mkNamedKeymap conf ks

    wsIndices = [ 1, 5, 2, 3, 4, 6, 7, 8, 0 ]
    wsKeys = map show $ wsIndices
    screenKeys = ["q","w"]
    dirKeys = ["j","k","h","l"]
    arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
    fulldirKeys = ["j", "<D>", "k", "<U>", "h", "<L>", "l", "<R>"]
    fulldirs = [D,D,U,U,L,L,R,R]
    dirs = [D,U,L,R]
    rstrdirs = [L,R]

    -- zip commands
    zipM m nm ks as f = zipWith (\k d -> (m++k, addName nm $ f d)) ks as
    zipM' m nm ks as f b = zipWith (\k d -> (m++k, addName nm $ f d b)) ks as


    in
    ---------------------------------------------------------------------------
    -- System / Utilities                                                   {{{
    ---------------------------------------------------------------------------
    subKeys "system"
    [ 
      ("M-x M-r"           , addName "restart XMonad"           $ spawn "xmonad --restart"                                                     )
    , ("M-x M-S-r"         , addName "rebuild & restart XMonad" $ spawn "xmonad --recompile && xmonad --restart"                               )
    , ("M-x M-e"           , addName "edit xmonad.hs"           $ spawn (myEditor ++ " ~/.xmonad/xmonad.hs")                                   )
    , ("M-x M-l"           , addName "lock screen"              $ spawn "physlock"                                                             )
    , ("M1-C-l"            , addName "lock screen"              $ spawn "physlock"                                                             )
    , ("M-x M-q"           , addName "Quit XMonad"              $ confirmPrompt hotPromptTheme "Quit XMonad"      $ io (exitWith ExitSuccess)  )
    ] ^++^

    ------------------------------------------------------------------------}}}
    -- Actions                                                              {{{
    ---------------------------------------------------------------------------
    subKeys "actions"
    [
    -- sound
      ("M-<Page_Up>"             , addName "volume +5%"                  $ spawn "amixer set Master 5%+ unmute" )
    , ("M-<Page_Down>"           , addName "volume -5%"                  $ spawn "amixer set Master 5%- unmute" )
    , ("M-<End>"                 , addName "mute/unmute"                 $ spawn "amixer -q set Master toggle"  )
    , ("<XF86AudioRaiseVolume>"  , addName "volume +5%"                  $ spawn "amixer set Master 5%+ unmute" )
    , ("<XF86AudioLowerVolume>"  , addName "volume -5%"                  $ spawn "amixer set Master 5%- unmute" )
    , ("<XF86AudioMute>"         , addName "mute/unmute"                 $ spawn "amixer set Master toggle"     )
    -- brightness
    , ("<XF86MonBrightnessDown>"   , addName "brightness -5"  $ spawn "light -U 5"    )
    , ("<XF86MonBrightnessUp>"     , addName "brightness +5"  $ spawn "light -A 5"    )
    , ("M-<XF86MonBrightnessDown>" , addName "brightness min" $ spawn "light -S 5"    )
    , ("M-<XF86MonBrightnessUp>"   , addName "brightness max"  $ spawn "light -S 100" )
    -- screenshots
    , ("<Print>"                 , addName "screenshot window"           $ spawn "scrot -u \"%Y-%m-%d-%r.jpg\" -e 'mv \"$f\" ~/Pictures/screenshots/.'")
    , ("M-<Print>"               , addName "screenshot fullscreen"       $ spawn "scrot \"%Y-%m-%d-%r.jpg\" -e 'mv \"$f\" ~/Pictures/screenshots/.'")
    , ("M-C-<Print>"             , addName "screenshot region"           $ spawn "sleep 0.5; scrot -s \"%Y-%m-%d-%r.jpg\" -e 'mv \"$f\" ~/Pictures/screenshots/.'") --sleep 0.5 is to avoid keypress cancel
    ] ^++^

    ------------------------------------------------------------------------}}}
    -- Launchers                                                            {{{
    ---------------------------------------------------------------------------
    subKeys "launchers"
    [ ("M-p"          , addName "launcher"      $ spawn myLauncher                                                      )
    , ("M-S-p"        , addName "launcher"      $ spawn myAltLauncher                                                   )
    , ("M-/"          , addName "window search" $ spawn myWinSearch                                                     )
    , ("M-<Return>"   , addName "terminal"      $ spawn myTerminal                                                      )
    , ("M-S-<Return>" , addName "alt-terminal"  $ spawn myAltTerminal                                                   )
    , ("M-\\"         , addName "browser"       $ runOrRaise myBrowser (className =? "Firefox")                         )
    , ("M-s"          , addName "ssh"           $ spawn "rofi-ssh"                                                      )
    , ("M-z"          , addName "logout"        $ spawn "rofi-session"                                                  )
    , ("M-S-o"        , addName "launcher"      $ spawn myAltLauncher                                                   )
    , ("M-o M-o"      , addName "launcher"      $ spawn myLauncher                                                      )
    , ("M-o M-b"      , addName "browser"       $ spawn myBrowser                                                       )
    , ("M-o M-S-b"    , addName "alt-browser"   $ spawn myAltBrowser                                                    )
    , ("M-o M-f"      , addName "files"         $ spawn myFiles                                                         )
    , ("M-o M-t"      , addName "terminal"      $ spawn myTerminal                                                      )
    , ("M-o M-S-T"    , addName "alt-terminal"  $ spawn myAltTerminal                                                   )
    , ("M-m"          , addName "Music"         $ runOrRaise myMusic (className =? "Google Play Music Desktop Player")  )
    ] ^++^
    ------------------------------------------------------------------------}}}
    -- Windows                                                              {{{
    ---------------------------------------------------------------------------
    subKeys "Windows"
    (
    [ ("M-<Backspace>" ,  addName "kill"                   $ kill                                                       )
    , ("M-C-<Backspace>", addName "kill all"               $ confirmPrompt hotPromptTheme "kill all windows?" $ killAll )
    , ("M-g M-m" ,        addName "Focus Master"           $ windows W.focusMaster                                      )
    , ("M-g M-n" ,        addName "Focus Urgent"           $ focusUrgent                                                )
    , ("M-g M-t",         addName "toggle floating window" $ withFocused toggleFloat                                    )
    , ("M-u" ,            addName "Tabs D"                 $ onGroup W.focusDown'                                       )
    , ("M-i" ,            addName "Tabs U"                 $ onGroup W.focusUp'                                         )
    , ("M-g M-g" ,        addName "Unmerge"                $ withFocused (sendMessage . UnMerge)                        )
    ] 
    ++ zipM' "M-"     "navigate window"           fulldirKeys fulldirs windowGo True
    ++ zipM' "M-S-"   "move window"               fulldirKeys fulldirs windowSwap True
    ++ zipM  "M-C-"   "merge w/sublayout"         fulldirKeys fulldirs (sendMessage . pullGroup)
    ++ zipM' "M-"     "navigate screen"           screenKeys  rstrdirs screenGo True
    ++ zipM' "M-S-"   "move window to screen"     screenKeys  rstrdirs windowToScreen True
    ++ zipM' "M-C-"   "Swap workspaces to screen" screenKeys  rstrdirs screenSwap True
    ) ^++^
    ------------------------------------------------------------------------}}}
    -- Workspaces                                                           {{{
    ---------------------------------------------------------------------------
    subKeys "workspaces"
    (
    [ ("M-' M-n", addName "next non-empty workspace" $ nextHidWS             )
    , ("M-' M-p", addName "prev non-empty workspace" $ prevHidWS             )
    , ("M-' M-'", addName "select workspace" $ selectWorkspace myPromptTheme )
    ]
    ++ zipM "M-"     "view workspace"           wsKeys [0..] (withNthWorkspace W.greedyView)
    ++ zipM "M-S-"   "move window to workspace" wsKeys [0..] (withNthWorkspace W.shift)
    ++ zipM "M-y M-" "copy window to workspace" wsKeys [0..] (withNthWorkspace copy)
    ) ^++^
    ------------------------------------------------------------------------}}}
    -- Layouts and SubLayouts                                               {{{
    ---------------------------------------------------------------------------
    subKeys "layouts"
    [ ("M-<Tab>",   addName "cycle all layouts"         $ sendMessage NextLayout                                           )
    , ("M-S-<Tab>", addName "cycle sublayout"           $ toSubl NextLayout                                                )
    , ("M-C-<Tab>", addName "reset layout"              $ setLayout $ XMonad.layoutHook conf                               )
    , ("M-S-t",     addName "tile all floating windows" $ sinkAll                                                          )
    , ("M-S-=",     addName "fullscreen"                $ sequence_ [ (withFocused $ windows . W.sink)
                                                                    , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL)
                                                                    ]                                                      )
    , ("M-' M-,",   addName "Decrease master windows"  $ sendMessage (IncMasterN (-1))                                     )
    , ("M-' M-.",   addName "Increase master windows"  $ sendMessage (IncMasterN 1)                                        )
    , ("M-' M-j",   addName "Shrink master"            $ sendMessage (Shrink)                                              )
    , ("M-' M-k",   addName "Expand master"            $ sendMessage (Expand)                                              )
    ] ^++^
    ------------------------------------------------------------------------}}}
    -- Scratchpads                                                          {{{
    ---------------------------------------------------------------------------
    subKeys "scratchpads"
    [ ("M-r", addName "htop"   $ namedScratchpadAction myScratchpads "htop"   )
    , ("M-t", addName "task"   $ namedScratchpadAction myScratchpads "task"   )
    , ("M-v", addName "mixer"  $ namedScratchpadAction myScratchpads "mixer"  )
    , ("M-e", addName "ranger" $ namedScratchpadAction myScratchpads "ranger" )
    ]
    ------------------------------------------------------------------------}}}

----------------------------------------------------------------------------}}}
-- Startup Hook                                                             {{{
-------------------------------------------------------------------------------
myStartupHook = do
    spawnOnce "xsetroot -cursor_name left_ptr" -- removing cross cursor
    spawnOnce "~/.config/fehbg" -- feh + xrandr script
    spawnOnce "picom"
    spawnOnce "dunst"
    XMonad.Hooks.DynamicBars.dynStatusBarStartup myBarCreator myBarDestroyer
    spawnOnce "Discord"
    spawnOnce "Slack"
    spawnOnce "google-play-music-desktop-player"

quitXmonad :: X ()
quitXmonad = io (exitWith ExitSuccess)

rebuildXmonad :: X ()
rebuildXmonad = 
    spawn "xmonad --recompile && xmonad --restart"

restartXmonad :: X ()
restartXmonad = 
    spawn "xmoand --restart"

----------------------------------------------------------------------------}}}
-- Log Hook                                                                 {{{
-------------------------------------------------------------------------------
myLogHook = do
    -- LogHook for multiple screens
    -- https://github.com/jonascj/.xmonad/blob/master/xmonad.hs 
    multiPP myLogPP myLogPP
    ewmhDesktopsLogHook
    fadeWindowsLogHook myFadeHook

myLogPP :: XMonad.Hooks.DynamicLog.PP
myLogPP = myXmobarLogPP

myXmobarLogPP :: XMonad.Hooks.DynamicLog.PP
myXmobarLogPP = def
    { ppCurrent = xmobarColor blue "" . clickableWorkspaces
    , ppTitle   = xmobarColor green "" . shorten 60
    , ppVisible = xmobarColor blue "" . clickableWorkspaces
    , ppUrgent  = xmobarColor red "" . clickableWorkspaces
    , ppHidden  = xmobarColor white "" . clickableWorkspaces
    , ppHiddenNoWindows = xmobarColor base01 "" . clickableWorkspaces
    , ppSep     = " <fn=1>\xf101</fn> "
    , ppWsSep   = " "
    , ppLayout  = xmobarColor yellow ""
    , ppSort    = mkWsSort wsCompare
    }
        where
            fixIndex :: Maybe Int -> Int
            fixIndex Nothing = 9
            fixIndex (Just a) = a

            wsOrder :: [WorkspaceId]
            wsOrder = [wsmain, wstex, wscode, wsgame, wswww, wscom, wsmedia, wssys, wsmin]

            wsIndex :: WorkspaceId -> Int
            wsIndex "min" = 10
            wsIndex a = fixIndex $ flip elemIndex wsOrder a

            wsCompare :: X WorkspaceCompare
            wsCompare = return (compare `on` wsIndex)

            workspaceToIcons :: String -> String
            workspaceToIcons "main"  = "<fn=1>\xf0f2</fn>" -- 
            workspaceToIcons "latex" = "<fn=1>\xf70e</fn>" -- 
            workspaceToIcons "code"  = "<fn=1>\xf121</fn>" -- 
            workspaceToIcons "game"  = "<fn=2>\xf1b6</fn>" -- 
            workspaceToIcons "www"   = "<fn=2>\xf269</fn>" --  
            workspaceToIcons "com"   = "<fn=1>\xf075</fn>" -- 
            workspaceToIcons "media" = "<fn=2>\xf3b5</fn>" -- 
            workspaceToIcons "sys"   = "<fn=1>\xf120</fn>" -- 
            workspaceToIcons "NSP"   = "<fn=1>\xf328</fn>" -- 
            workspaceToIcons _       = "<fn=1>\xf714</fn>" -- 

            clickableWorkspaces :: String -> String
            clickableWorkspaces ws = "<action=xdotool key Super+" ++ show ((wsIndex ws) + 1) ++">" ++ workspaceToIcons ws ++ "</action>"

-- Defining barcreator and destroyer
myBarCreator   = xmobarCreator
myBarDestroyer = xmobarDestroyer

-- Xmobar Creator and Destroyer using dynamic bars
xmobarCreator :: XMonad.Hooks.DynamicBars.DynamicStatusBar
xmobarCreator (XMonad.S sid) = do
    t <- XMonad.liftIO Data.Time.LocalTime.getZonedTime
    XMonad.trace (show t ++ ": XMonad xmobarCreator " ++ show sid) --logging
    XMonad.Util.Run.spawnPipe ("~/.config/xmobar/xmobar -x " ++ show sid)

xmobarDestroyer :: XMonad.Hooks.DynamicBars.DynamicStatusBarCleanup
xmobarDestroyer = do
    t <- XMonad.liftIO Data.Time.LocalTime.getZonedTime
    XMonad.trace (show t ++ ": XMonad xmobarDestroyer") -- logging

-- FadeHook
myFadeHook = composeAll
    [ opaque
    , isUnfocused          --> opacity 0.95
    , isDialog             --> opaque
    , isFloating           --> opacity 0.85
    , isRole =? "browser"  --> opacity 1
    , isFullscreen         --> opacity 1
    , className =? "vlc"   --> opacity 1
    , className =? "feh"   --> opacity 1
    , className =? "dota2" --> opacity 1
    ]
        where
            isRole = stringProperty "WM_WINDOW_ROLE"

----------------------------------------------------------------------------}}}
-- Manage Hook                                                              {{{
-------------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = myCustomManageHook
    <+> XMonad.Hooks.ManageDocks.manageDocks -- Docks ManageHook
    <+> namedScratchpadManageHook myScratchpads -- Spawning and managing scratchpads
    <+> positionStoreManageHook (Just defaultThemeWithImageButtons)
    <+> XMonad.Layout.Fullscreen.fullscreenManageHook
    <+> manageHook def

myCustomManageHook :: ManageHook
myCustomManageHook = composeAll . concat $
    [ [ className =? c <||> title=? c --> doF (W.shift wswww)                            |  c <- myWebShifts   ]
    , [ className =? c                --> doF (W.view wsgame)                            |  c <- myGameShifts  ]
    , [ className =? c                --> doF (W.shift wsgame)                           |  c <- myGameShifts  ]
    , [ className =? c                --> doF (W.shift wscom)                            |  c <- myComShifts   ]
    , [ className =? c                --> doF (W.shift wsmedia)                          |  c <- myMediaShifts ]
    , [ className =? c                --> doF (W.view wsmedia)                           |  c <- myMediaViews  ]
    , [ className =? c                --> doF (W.shift wsmedia)                          |  c <- myMediaViews  ]
    , [ className =? c                --> doCenterFloat                                  |  c <- myCFloats     ]
    , [ className =? c                --> doRectFloat (W.RationalRect 0.73 0 0.25 0.25)  |  c <- myRFloats     ]
        -- Handling specific conditions
    , [ isFullscreen --> doFullFloat ]
    , [ isDialog --> doCenterFloat ]
    , [ isRole =? "pop-up" --> doCenterFloat ]
    ]
        where
            isRole = stringProperty "WM_WINDOW_ROLE"
            myWebShifts = ["Firefox", "google-chrome"]
            myGameShifts = ["dota2", "Steam"]
            myComShifts = ["Slack", "Discord", "weechat"]
            myMediaViews = ["vlc"]
            myMediaShifts = ["Google Play Music Desktop Player"]
            myCFloats = ["feh"]
            myRFloats = ["ikhal"]

----------------------------------------------------------------------------}}}
-- Event Hook                                                               {{{
-------------------------------------------------------------------------------

myHandleEventHook =
    XMonad.Hooks.ManageDocks.docksEventHook -- Handle dock events
    <+> XMonad.Hooks.DynamicBars.dynStatusBarEventHook myBarCreator myBarDestroyer -- Create dynamic status bars
    <+> ewmhDesktopsEventHook
	<+> positionStoreEventHook
    <+> XMonad.Layout.Fullscreen.fullscreenEventHook
    <+> handleEventHook def

----------------------------------------------------------------------------}}}
-- Urgency Hook                                                             {{{
-------------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        let cmd = "notify-send -a Urgent \"Workspace " ++ idx ++ "\" \"" ++ (show name) ++ "\""
        spawn cmd

----------------------------------------------------------------------------}}}

-- vim: ft=haskell:foldmethod=marker:expandtab:ts=4:sts=4:shiftwidth=4
