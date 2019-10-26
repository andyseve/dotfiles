{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
-- Author: Anish Sevekari
-- Last Modified: Sat 26 Oct 2019 09:23:49 AM EDT
-- Based on : https://github.com/altercation
--
-- TODO                                                                     {{{
-------------------------------------------------------------------------------


----------------------------------------------------------------------------}}}
-- Modules                                                              {{{
---------------------------------------------------------------------------
--import Control.Monad (liftM2)             -- myManageHookShift
import Control.Monad (liftM, liftM2, join)  -- myManageHookShift
import Data.List
import qualified Data.Map as M
import Data.Monoid
import System.Exit
import System.IO                            -- for xmonbar
import System.Posix.Process(executeFile)
import System.IO.Unsafe                     -- for modes
import System.Posix.Unistd                  -- for hostname
import Data.Time.LocalTime

import XMonad hiding ( (|||) )              -- ||| from X.L.LayoutCombinators
import qualified XMonad.StackSet as W       -- myManageHookShift

import XMonad.Actions.Commands
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow            -- like cylons, except x windows
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatSnap
import XMonad.Actions.MessageFeedback       -- pseudo conditional key bindings
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote               -- promote window to master
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn
--import XMonad.Actions.Volume
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll               -- action all the things
import XMonad.Actions.KeyRemap              -- this may let me do a hackity hack hack to get vim-like modes!

import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog              -- for xmobar
import XMonad.Hooks.DynamicProperty         -- 0.12 broken; works with github version
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks             -- avoid xmobar
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName

--import XMonad.Layout hiding ( (|||) )       -- ||| from X.L.LayoutCombinators
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.Column
import XMonad.Layout.Combo
import XMonad.Layout.ComboP
import XMonad.Layout.DecorationMadness      -- testing alternative accordion styles
import XMonad.Layout.Dishes
import XMonad.Layout.DragPane
import XMonad.Layout.Drawer
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.OneBig
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.PerWorkspace           -- Configure layouts on a per-workspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile          -- Resizable Horizontal border
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing                -- this makes smart space around windows
import XMonad.Layout.StackTile
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts          -- Full window at any time
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Layout.GridVariants (Grid(Grid))

import XMonad.Prompt                        -- to get my old key bindings working
import XMonad.Prompt.ConfirmPrompt          -- don't just hard quit

import XMonad.Util.Cursor
import XMonad.Util.EZConfig                 -- removeKeys, additionalKeys
import XMonad.Util.Loggers
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Paste as P               -- testing
import XMonad.Util.Run                      -- for spawnPipe and hPutStrLn
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare         -- custom WS functions filtering NSP
import XMonad.Util.XSelection


-- experimenting with tripane
import XMonad.Layout.Decoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders

-- taffybar specific
-- import System.Taffybar.Hooks.PagerHints (pagerHints)
-- to demo and comment out or remove
-- import XMonad.Layout.Master -- used to test a dynamic layout. worked, but will remove in lieu of sublayouts
-- import XMonad.Actions.CycleSelectedLayouts -- nice but doesn't work well with sublayouts
-- import XMonad.Actions.Plane
-- import XMonad.Layout.IndependentScreens
-- import XMonad.Util.Timer
-- recent windows from cycle windows -- couldn't get it working on quick try: revisit this
-- import XMonad.Actions.CycleWindows
-- testing -- not a lot of value added, or am I missing something
-- import XMonad.Hooks.Place
----
-- following for the combocombo test from
-- http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Droundy.html
-- import XMonad.Layout.Square ( Square(Square) )
-- import XMonad.Layout.BoringWindows
-- import XMonad.Layout.Grid
----
-- import XMonad.Layout.SimpleDecoration
-- testing -- couldn't get this to work
-- import XMonad.Layout.TrackFloating
-- testing
-- import XMonad.Hooks.ServerMode
-- import XMonad.Actions.Commands 
-- import Control.Concurrent (threadDelay)

------------------------------------------------------------------------}}}
-- Main                                                                 {{{
---------------------------------------------------------------------------

main = do
    xmonad
        $ withNavigation2DConfig myNav2DConf
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
    , mouseBindings      = myMouseBindings
    , startupHook        = myStartupHook
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    }

----------------------------------------------------------------------------}}}
-- Workspaces                                                               {{{
-------------------------------------------------------------------------------

ws1   = "main"
ws2   = "www"
ws3   = "TeX"
ws4   = "aux1"
ws5   = "aux2"
ws6   = "chat"
ws7   = "media"
ws8   = "mail"
ws9   = "sus"
wsmin = "min"

myWorkspaces :: [String]
myWorkspaces = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, "NSP"]	

----------------------------------------------------------------------------}}}
-- Applications                                                             {{{
-------------------------------------------------------------------------------

myTerminal    = "alacritty"
myAltTerminal = "gnome-terminal"
myBrowser     = "firefox"
myAltBrowser  = "google-chrome"
myLauncher    = "rofi-run"
myAltLauncher = "dmenu_run"
myKeyViewer   = "rofi -i -dmenu -p 'Xmonad keys'"
myWinSearch   = "rofi-window"
myStatusBar   = "xmobar -x  /home/stranger/.xmonad/xmobar.conf"
myFiles       = "nautilus ~"
myEditor      = "gvim"

scratchpads = [ 
                NS "htop" spawnHtop findHtop manageHtop
              , NS "weather" spawnWeather findWeather manageWeather
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
    spawnKeepass  = "keepassxc"
    findKeepass   = resource =? "keepassxc"
    manageKeepass = customFloating $ W.RationalRect x y w h
                 where
                   w, h, x, y :: Rational
                   h = 8/10
                   w = 1/2
                   x = (1-w)/2
                   y = (1-h)/2
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

-- sizes
gap    = 1
topbar = 2
border = 2
prompt = 20
status = 20

myNormalBorderColor  = "#000000"
myFocusedBorderColor = active

active       = blue
activeWarn   = red
inactive     = base02
focuscolor   = blue
unfocuscolor = base02

mySmallFont = "xft:hack:style=Regular:size=8:hinting=true"
myFont      = "xft:hack:style=Regular:size=10:hinting=true"
myBigFont   = "xft:hack:style=Regular:size=12:hinting=true"

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

------------------------------------------------------------------------}}}
-- Layouts                                                              {{{
--------------------------------------------------------------------------
myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full",          centerNavigation)
    -- line/center same results   ,("Simple Tabs", lineNavigation)
    --                            ,("Simple Tabs", centerNavigation)
                                  ]
    , unmappedWindowRect        = [("Full", singleWindowRect)
    -- works but breaks tab deco  ,("Simple Tabs", singleWindowRect)
    -- doesn't work but deco ok   ,("Simple Tabs", fullScreenRect)
                                  ]
    }


data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)

-- tabBarFull = avoidStruts $ noFrillsDeco shrinkText topBarTheme $ addTabs shrinkText myTabTheme $ Simplest
barFull = avoidStruts $ Simplest

-- cf http://x

--myLayoutPrompt = inputPromptWithCompl myPromptTheme "Layout"
                 --(mkComplFunFromList' allLayouts)
                 -- ?+ (sendMessage . JumpToLayout)
--allLayouts = ["tall", "wide", "circle", "full", "tabbed", "accordion"]

myLayoutHook = fullscreenFloat -- fixes floating windows going full screen, while retaining "bounded" fullscreen
             $ showWorkspaceName
             $ fullScreenToggle
             $ fullBarToggle
             $ mirrorToggle
             $ reflectToggle
             $ onWorkspace "float" floatWorkSpace
             $ onWorkspace "www" tabWorkSpace
             $ onWorkspace "sys" tabWorkSpace
             $ onWorkspace "TeX" texWorkSpace
             $ onWorkspace "mail" texWorkSpace
             $ onWorkspace "music" musicWorkSpace
             $ flex ||| tabs
  where

--    testTall = Tall 1 (1/50) (2/3)
--    myTall = subLayout [] Simplest $ trackFloating (Tall 1 (1/20) (1/2))

    floatWorkSpace      = floaty
    texWorkSpace        = flexr ||| tabs
    tabWorkSpace        = tabs ||| flex
    musicWorkSpace      = mirrored ||| monocle
    fullBarToggle       = mkToggle (single FULLBAR)
    fullScreenToggle    = mkToggle (single FULL)
    mirrorToggle        = mkToggle (single MIRROR)
    reflectToggle       = mkToggle (single REFLECTX)
    smallMonResWidth    = 1920
    showWorkspaceName   = showWName' myShowWNameTheme

    named n             = renamed [(XMonad.Layout.Renamed.Replace n)]
    trimNamed w n       = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                                   (XMonad.Layout.Renamed.PrependWords n)]
    suffixed n          = renamed [(XMonad.Layout.Renamed.AppendWords n)]
    trimSuffixed w n    = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                                   (XMonad.Layout.Renamed.AppendWords n)]

    addTopBar           = noFrillsDeco shrinkText topBarTheme

    mySpacing           = spacing gap
    sGap                = quot gap 2
    myGaps              = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
    mySmallGaps         = gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]
    myBigGaps           = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]

    --------------------------------------------------------------------------
    -- Tabs Layout                                                          --
    --------------------------------------------------------------------------

    threeCol = named "unflexed"
         $ avoidStruts
         $ addTopBar
         $ myGaps
         $ mySpacing
         $ ThreeColMid 1 (1/10) (1/2)

    tabs = named "tabs"
         $ avoidStruts
         $ addTopBar
         $ addTabs shrinkText myTabTheme
         $ Simplest

    --------------------------------------------------------------------------
    -- Float Layout                                                         --
    --------------------------------------------------------------------------

    floaty = named "float"
        $ addTopBar
        $ simplestFloat

    --------------------------------------------------------------------------
    -- Full Layout                                                          --
    --------------------------------------------------------------------------
        -- not working yet since I don't yet have a way to navigate between windows...

    monocle = named "monocle"
            $ avoidStruts
            $ addTopBar
            $ subLayout [] (Simplest ||| Accordion)
            $ Full

    mirrored = named "mirrored"
             $ avoidStruts
             $ addTopBar
             $ windowNavigation
             $ Mirror (Tall 1 (3/100) (1/10))


    -----------------------------------------------------------------------
    -- Flexi SubLayouts                                                  --
    -----------------------------------------------------------------------
    --
    -- In many ways the best solution. Acts like ThreeColumns, Tall, BSP,
    -- or any other container layout style. Can use this layout just as you
    -- would those without tabs at all, or you can easily merge any windows
    -- into a tabbed group.
    --
    -- Diagrams:
    --
    -- (examples only... this is a very flexible layout and as such the
    -- layout style and arrangement isn't limited as much as the other
    -- attempts below)
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |   Tabs   |
    -- |          |                    |          |
    -- |----------|       Master       |----------|
    -- |          |                    |          |
    -- |   Tabs   |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |----------|
    -- |                    |          |
    -- |                    |   Tabs   |
    -- |                    |          |
    -- ---------------------------------
    --
    --
    -- Advantages
    --
    --   * tab group is movable as a unit and acts like any other window
    --
    --   * this is the "cleanest" of the dynamic layouts I've worked with
    --     and leaves no "pixel dust" on the screen when switching to a WS
    --     on a different monitor
    --
    --   * navigation and window/group movement is trivial with
    --     X.A.Navigation2D
    --
    --   * master window remains master when switching screens (unlike
    --     the "X.L.Master" based solution below)
    --
    --   * unlike some of the other solutions, it is trivial to change
    --     the exterior layout format and so I could potentially add in
    --     some layout change to BSP or other layout that I want to test
    --     while still retaining the tab functionality
    --
    -- Disadvantages
    --
    --   * layout starts without any tabs (could be considered a feature
    --     since in that case the layout performs exactly as the parent/
    --     container layout does)
    --
    --   * To move a window into or out of the tabbed group requires
    --     special key bindings unique to X.L.SubLayouts
    --
    --  Understanding XMonad.Layouts.SubLayouts
    --
    --  It took me a while to grok this.
    --
    --  the subLayout hook is used with the following format:
    --
    --    subLayout advanceInnerLayouts innerLayout outerLayout
    --
    --  It works like this: subLayout modifies an entire other layout (or
    --  layouts), enabling you to turn what would be a normal window into
    --  a little group of windows managed by an entirely different layout.
    --
    --  In my case, I'm using layouts like "Three Column" and "Tall" as the
    --  nominal "container" layout (what SubLayouts calls the "outerLayout").
    --
    --  The "inner layout" in my case is just "Simplest". I'm also adding tabs
    --  which are only applied to my sublayouts. Not sure how that works
    --  but it's apparent from the X.L.SubLayouts documentation that this is
    --  the intended use/behavior. Essential X.L.SubLayouts is hijacking these
    --  added tabs and applying them just to the Simplest layout, and then that
    --  in turn is stuck inside the rectangle that would normally hold a window
    --  in my normal layouts.
    --
    --  One of the confusing things for me at first was that the layout doesn't
    --  start with any subLayouts. So it appears to just be a normal layout.
    --  You have to "merge all" to suck everything up into a Simplest tabbed
    --  group and then you can add other windows normally and you'll
    --  have a sublayout with tabs.
    --
    --  Note: subLayouts has some other features. For example, you can give it
    --  a list of layouts to work through and it will advance through them in
    --  series (or possibly in an order your provide) and will apply different
    --  layouts to different subLayout groups. Each time you add a new window
    --  to your layout, it acquires the sublayout, even if you don't know it.
    --
    --  In my case, my list is one long and is just the first window I add.
    --
    --  Ex. The second group is Tall, the third is Circle, all others are
    --  tabbed with:
    --
    --  myLayout = addTabs shrinkText def
    --           $ subLayout [0,1,2] (Simplest ||| Tall 1 0.2 0.5 ||| Circle)
    --                    $ Tall 1 0.2 0.5 ||| Full
   
    -- this is a flexible sublayout layout that has only one container
    -- layout style (depending on screen)
    --     flexiSub = named "Flexi SubLayouts"
    --               $ avoidStruts
    --               $ windowNavigation
    --               $ addTopBar
    --               $ myGaps
    --               $ addTabs shrinkText myTabTheme
    --               $ mySpacing
    --               $ subLayout [] Simplest
    --               $ ifWider smallMonResWidth wideLayout standardLayout
    --               where
    --                   wideLayout = ThreeColMid 1 (1/100) (1/2)
    --                   standardLayout = ResizableTall 1 (1/50) (2/3) []

    -- retained during development: safe to remove later

    flex = trimNamed 5 "flex"
              $ avoidStruts
              -- don't forget: even though we are using X.A.Navigation2D
              -- we need windowNavigation for merging to sublayouts
              $ windowNavigation
              $ addTopBar
              $ addTabs shrinkText myTabTheme
              -- $ subLayout [] (Simplest ||| (mySpacing $ Accordion))
              $ subLayout [] (Simplest ||| Accordion)
              $ ifWider smallMonResWidth wideLayouts standardLayouts
              where
                  wideLayouts = myGaps $ mySpacing
                      $ (suffixed "3col" $ ThreeColMid 1 (1/20) (1/2))
                    ||| (suffixed "BSP"  $ hiddenWindows emptyBSP)
                    -- ||| (suffixed "grid" $ Grid (16/10))
                    -- ||| fullTabs
                  standardLayouts = myGaps $ mySpacing
                      $ (suffixed "1/2"  $ ResizableTall 1 (1/20) (1/2) [])
                    ||| (suffixed "2/3"  $ ResizableTall 1 (1/20) (2/3) [])
                    -- ||| (suffixed "grid" $ Grid (16/10))

    flexr = trimNamed 5 "flex"
              $ avoidStruts
              -- don't forget: even though we are using X.A.Navigation2D
              -- we need windowNavigation for merging to sublayouts
              $ windowNavigation
              $ addTopBar
              $ addTabs shrinkText myTabTheme
              -- $ subLayout [] (Simplest ||| (mySpacing $ Accordion))
              $ subLayout [] (Simplest ||| Accordion)
              $ ifWider smallMonResWidth wideLayouts standardLayouts
              where
                  wideLayouts = myGaps $ mySpacing
                      $ (suffixed "3col" $ ThreeColMid 1 (1/20) (1/2))
                    ||| (suffixed "BSP"  $ hiddenWindows emptyBSP)
                    -- ||| (suffixed "grid" $ Grid (16/10))
                    -- ||| fullTabs
                  standardLayouts = myGaps $ mySpacing
                      $ (suffixed "2/3"  $ ResizableTall 1 (1/20) (2/3) [])
                    ||| (suffixed "1/2"  $ ResizableTall 1 (1/20) (1/2) [])
                  --  ||| fullTabs
                  --fullTabs = suffixed "Tabs Full" $ Simplest
                  --
                  -- NOTE: removed this from the two (wide/std) sublayout
                  -- sequences. if inside the ifWider, the ||| combinator
                  -- from X.L.LayoutCombinators can't jump to it directly (
                  -- or I'm doing something wrong, either way, it's simpler
                  -- to solve it by just using a tabbed layout in the main
                  -- layoutHook). The disadvantage is that I lose the "per
                  -- screen" memory of which layout was where if using the
                  -- tabbed layout (if using the the ifWider construct as
                  -- I am currently, it seems to work fine)
                  --
                  -- Using "Full" here (instead of Simplest) will retain the
                  -- tabbed sublayout structure and allow paging through each
                  -- group/window in full screen mode. However my preference
                  -- is to just see all the windows as tabs immediately.  
                  -- Using "Simplest" here will do this: display all windows
                  -- as tabs across the top, no "paging" required. However
                  -- this is misleading as the sublayouts are of course still
                  -- there and you will have to use the nornmal W.focusUp/Down
                  -- to successfully flip through them. Despite this
                  -- limitation I prefer this to the results with "Full".

    --grid = named "grid"
         -- $ avoidStruts
         -- $ addTopBar
         -- $ myGaps
         -- $ mySpacing
         -- $ windowNavigation
         -- $ addTabs shrinkText myTabTheme
         -- $ subLayout [] (Simplest ||| Accordion)
         -- $ Grid (16/10)

{-|
    -----------------------------------------------------------------------
    -- Simple Flexi                                                      --
    -----------------------------------------------------------------------
    --
    -- Simple dynamically resizing layout as with the other variations in
    -- this config. This layout has not tabs in it and simply uses
    -- Resizable Tall and Three Column layouts.

    simpleFlexi = named "Simple Flexible"
              $ ifWider smallMonResWidth simpleThree simpleTall

    simpleTall = named "Tall"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ ResizableTall 1 (1/300) (2/3) []
              
    simpleThree = named "Three Col"
              $ avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ ThreeColMid 1 (3/100) (1/2)

    -----------------------------------------------------------------------
    -- Other Misc Layouts                                                --
    -----------------------------------------------------------------------
    --
    --

    masterTabbedP   = named "MASTER TABBED"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ mastered (1/100) (1/2) $ tabbed shrinkText myTabTheme

    bsp       = named "BSP"
              $ borderResize (avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ emptyBSP )
              -- $ borderResize (emptyBSP)

    oneBig    = named "1BG"
              $ avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ OneBig (3/4) (3/4)

    tiledP    = named "TILED"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ consoleOn
              $ tiled'

    oneUp =   named "1UP"
              $ avoidStruts
              $ myGaps
              $ combineTwoP (ThreeCol 1 (3/100) (1/2))
                            (Simplest)
                            (Tall 1 0.03 0.5)
                            (ClassName "Google-chrome-beta")

    -----------------------------------------------------------------------
    -- Master-Tabbed Dymamic                                             --
    -----------------------------------------------------------------------
    --
    -- Dynamic 3 pane layout with one tabbed panel using X.L.Master
    -- advantage is that it can do a nice 3-up on both ultrawide and
    -- standard (laptop in my case) screen sizes, where the layouts
    -- look like this:
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |  Master  |       Master       |   Tabs   |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    -- \____________________ _____________________/
    --                      '
    --                 all one layout
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |   Tabs   |
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- ---------------------------------
    -- \_______________ _______________/
    --                 '
    --            all one layout
    --
    -- Advantages to this use of X.L.Master to created this dynamic
    -- layout include:
    --
    --   * No fussing with special keys to swap windows between the
    --     Tabs and Master zones
    --
    --   * Window movement and resizing is very straightforward
    --
    --   * Limited need to maintain a mental-map of the layout
    --     (pretty easy to understand... it's just a layout)
    --
    -- Disadvantages include:
    --
    --   * Swapping a window from tabbed area will of necessity swap
    --     one of the Master windows back into tabs (since there can
    --     only be two master windows)
    --
    --   * Master area can have only one/two windows in std/wide modes
    --     respectively
    --
    --   * When switching from wide to standard, the leftmost pane
    --     (which is visually secondary to the large central master
    --     window) becomes the new dominant master window on the
    --     standard display (this is easy enough to deal with but
    --     is a non-intuitive effect)

    masterTabbedDynamic = named "Master-Tabbed Dynamic"
              $ ifWider smallMonResWidth masterTabbedWide masterTabbedStd

    masterTabbedStd = named "Master-Tabbed Standard"
              $ addTopBar
              $ avoidStruts
              $ gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]
              $ mastered (1/100) (2/3)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ tabbed shrinkText myTabTheme

    masterTabbedWide = named "Master-Tabbed Wide"
              $ addTopBar
              $ avoidStruts
              $ gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]
              $ mastered (1/100) (1/4)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ mastered (1/100) (2/3)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ tabbed shrinkText myTabTheme

    -----------------------------------------------------------------------
    -- Tall-Tabbed Dymamic                                               --
    -----------------------------------------------------------------------
    --
    -- Dynamic 3 pane layout with one tabbed panel using X.L.ComboP
    -- advantage is that it can do a nice 3-up on both ultrawide and
    -- standard (laptop in my case) screen sizes, where the layouts
    -- look like this:
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |----------|       Master       |   Tabs   |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    -- \______________ _______________/\____ _____/
    --                '                     '
    --        this set of panes is      This is a
    --        its' own layout in a      separate
    --        Tall configuration        tab format
    --                                  layout
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |   Tabs   |
    -- |                    |          |
    -- |--------------------|          |
    -- |         |          |          |
    -- ---------------------------------
    -- \_________ _________/\____ _____/
    --           '               '
    -- this set of panes is  This is a
    -- its' own layout in a  separate
    -- Tall configuration    tab format
    --                       layout
    --
    -- Advantages to this use of ComboP to created this dynamic
    -- layout include:
    --
    --   * the center Master stays the same when the layout
    --     changes (unlike the X.L.Master based dyn. layout)
    --
    --   * the Master can have a set of panes under it on the
    --     small screen (standard) layout
    --
    --   * on ultrawide the leftmost pane may be divided into
    --     multiple windows
    --
    --   * possible to toss a tabbed window to the "Master" area
    --     without swapping a window back into tabs
    --
    --   * use of ComboP allows redirection windows to either
    --     left or right section
    --
    -- Disadvantages include:
    --
    --   * normal window swaps fail between the two separate
    --     layouts. There must be a special swap-between-layouts
    --     binding (normal window NAVIGATION works, at least using
    --     X.A.Navigation2D).
    --
    --   * switching between screens can leave title bar clutter
    --     that hasn't been cleaned up properly (restarting
    --     XMonad works to clean this up, but that's hacky)
    --
    --   * somewhat greater need to maintain a mental-map of the
    --     layout (you need to have a sense for the windows being
    --     in separate sections of the different layouts)

    smartTallTabbed = named "Smart Tall-Tabbed"
            $ avoidStruts
            $ ifWider smallMonResWidth wideScreen normalScreen
            where
            wideScreen   = combineTwoP (TwoPane 0.03 (3/4))
                           (smartTall)
                           (smartTabbed)
                           --(ClassName "Google-chrome-beta")
            normalScreen = combineTwoP (TwoPane 0.03 (2/3))
                           (smartTall)
                           (smartTabbed)
                           --(ClassName "Google-chrome-beta")

    smartTall = named "Smart Tall"
            $ addTopBar
        $ mySpacing
            $ myGaps
        $ boringAuto
            $ ifWider smallMonResWidth wideScreen normalScreen
            where
                wideScreen = reflectHoriz $ Tall 1 0.03 (2/3)
                normalScreen = Mirror $ Tall 1 0.03 (4/5)

    smartTabbed = named "Smart Tabbed"
              $ addTopBar
              $ myCustomGaps
              $ tabbed shrinkText myTabTheme
-}
    -----------------------------------------------------------------------
    -- Flexi Combinators                                                 --
    -----------------------------------------------------------------------
    --
    -- failed attempt. creates a nice looking layout but I'm not sure
    -- how to actually direct tabs to the tabbed area
    --
    --     flexiCombinators = named "Flexi Combinators"
    --             $ avoidStruts
    --             $ ifWider smallMonResWidth wideScreen normalScreen
    --             where
    --             wideScreen   = smartTall ****||* smartTabbed
    --             normalScreen = smartTall ***||** smartTabbed




------------------------------------------------------------------------}}}
-- Bindings                                                             {{{
---------------------------------------------------------------------------

myModMask = mod4Mask -- super key (win)

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "show keybindings" $ io $ do
    --h <- spawnPipe "zenity --text-info --font='Hack 9'"
    h <- spawnPipe myKeyViewer
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

-- some of the structure of the following cribbed from 
-- https://github.com/SimSaladin/configs/blob/master/.xmonad/xmonad.hs
-- https://github.com/paul-axe/dotfiles/blob/master/.xmonad/xmonad.hs
-- https://github.com/pjones/xmonadrc (+ all the dyn project stuff)

wsKeys = map show $ [1..9] ++ [0]

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
        ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden


screenKeys     = [",", "."]
dirKeys        = ["j", "k", "h", "l"]
arrowKeys      = ["<D>", "<U>", "<L>", "<R>"]
fulldirKeys    = ["j", "<D>", "k", "<U>", "h", "<L>", "l", "<R>"]
fulldirs       = [D, D, U, U, L, L, R, R]
dirs           = [D, U, L, R]
rstrdirs       = [L, R]

--screenAction f        = screenWorkspace >=> flip whenJust (windows . f)

zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

-- from xmonad.layout.sublayouts
focusMaster' st = let (f:fs) = W.integrate st
    in W.Stack f [] fs
swapMaster' (W.Stack f u d) = W.Stack f [] $ reverse u ++ d

-- try sending one message, fallback if unreceived, then refresh
tryMsgR x y = sequence_ [(tryMessage_ x y), refresh]

-- warpCursor = warpToWindow (9/10) (9/10)

-- cf https://github.com/pjones/xmonadrc
--switch :: ProjectTable -> ProjectName -> X ()
--switch ps name = case Map.lookup name ps of
--  Just p              -> switchProject p
--  Nothing | null name -> return ()

-- do something with current X selection
unsafeWithSelection app = join $ io $ liftM unsafeSpawn $ fmap (\x -> app ++ " " ++ x) getSelection

toggleFloat w = windows (\s -> if M.member w (W.floating s)
                then W.sink w s
                else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

--myModeKeys = do
    --modefile <- openFile "~/.xmonad/mode" ReadMode
    --myMode <- hGetContents modefile
    --xmproc <- spawnPipe myStatusBar
    --case myMode of
        ----"normal" -> myNormKeys
        --"insert" -> myKeys $ myConfig xmproc
        ----"visual" -> myVisKeys
--myModeKeys = do
    --myMode <- unsafePerformIO . readFile $ "~/.xmonad/mode"
    --case myMode of
        --"insert" -> return myMode
        --
        --
--myMode = unsafePerformIO . readFile $ "~/.xmonad/mode"

--myModeKeys conf = do 
    ----let myMode = readFile "~/.xmonad/mode"
    --case myMode of
        --"insert" -> (subtitle "insert":) $ mkNamedKeymap conf $
            ----let subKeys str ks = subtitle str : mkNamedKeymap conf ks
            ----in

            ----subKeys "system"

            --[
              --("M-x M-r"                  , addName "restart XMonad"                  $ spawn myRestart)
            --, ("M-x M-S-r"                , addName "rebuild & restart XMonad"        $ spawn myReload)
            --, ("M-x M-s"                  , addName "switch to normal"                $ spawn "my-mode normal")
            --]
        --"normal" -> (subtitle "normal":) $ mkNamedKeymap conf $
            --[
              --("M-x M-r"                  , addName "restart XMonad"                  $ spawn myRestart)
            --, ("M-x M-S-r"                , addName "rebuild & restart XMonad"        $ spawn myReload)
            --, ("M-x M-s"                  , addName "switch to insert"                $ spawn "my-mode insert")
            --]
            --

              

myKeys conf = let

    subKeys str ks = subtitle str : mkNamedKeymap conf ks

    in

    -----------------------------------------------------------------------
    -- System / Utilities
    -----------------------------------------------------------------------
    subKeys "system"
    [ 
      ("M-x M-r"                  , addName "restart XMonad"                  $ spawn "xmonad --restart")
    , ("M-x M-S-r"                , addName "rebuild & restart XMonad"        $ spawn "xmonad --recompile && xmonad --restart")
    , ("M-x M-e"                  , addName "edit xmonad.hs"                  $ spawn (myEditor ++ " ~/.xmonad/xmonad.hs"))
    , ("M-x M-l"                  , addName "lock screen"                     $ spawn "gnome-screensaver-command -l")
    , ("M1-C-l"                   , addName "lock screen"                     $ spawn "gnome-screensaver-command -l")
    , ("M-F1"                     , addName "show keybindings"                $ return ())
    , ("M-x M-q"                  , addName "Quit XMonad"                     $ confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))
    ] ^++^

    -----------------------------------------------------------------------
    -- Actions
    -----------------------------------------------------------------------
    subKeys "actions"
    [
      ("M-<Page_Up>"                 , addName "volume +5%"                  $ spawn "amixer set Master 5%+ unmute")
    , ("M-<Page_Down>"               , addName "volume -5%"                  $ spawn "amixer set Master 5%- unmute")
    , ("M-<End>"                     , addName "mute/unmute"                 $ spawn "amixer set Master toggle")
    , ("<XF86AudioRaiseVolume>"      , addName "volume +5%"                  $ spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioLowerVolume>"      , addName "volume -5%"                  $ spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioMute>"             , addName "mute/unmute"                 $ spawn "amixer set Master toggle")
    , ("<Print>"                     , addName "screenshot"                  $ runOrRaise "gnome-screenshot" (className =? "gnome-screenshot"))
    , ("S-<Print>"                   , addName "screenshot fullscreen"       $ spawn "gnome-screenshot --fullscreen")
    , ("C-<Print>"                   , addName "screenshot region"           $ spawn "gnome-screenshot --region")
    , ("M1-<Print>"                  , addName "screenshot window"           $ spawn "gnome-screenshot --window")
    ] ^++^

    -----------------------------------------------------------------------
    -- Launchers
    -----------------------------------------------------------------------
    subKeys "launchers"
    [
      ("M-p"          , addName "launcher"                        $ spawn myLauncher)
    , ("M-S-p"        , addName "alt-launcher"                    $ spawn myAltLauncher)
    , ("M-/"          , addName "window search"                   $ spawn myWinSearch)
    , ("M-<Return>"   , addName "terminal"                        $ spawn myTerminal)
    , ("M-S-<Return>" , addName "alt-terminal"                    $ spawn myAltTerminal)
    , ("M-\\"         , addName "browser"                         $ spawn myBrowser)
    , ("M-s"          , addName "ssh"                             $ spawn "rofi-ssh")
    , ("M-e"          , addName "files"                           $ spawn myFiles)
    , ("M-S-o"        , addName "launcher"                        $ spawn myLauncher)
    , ("M-o M-o"      , addName "launcher"                        $ spawn myLauncher)
    , ("M-o M-b"      , addName "browser"                         $ spawn myBrowser)
    , ("M-o M-S-b"    , addName "alt-browser"                     $ spawn myAltBrowser)
    , ("M-o M-f"      , addName "files"                           $ spawn myFiles)
    , ("M-o M-s"      , addName "skype"                           $ spawn "skypeforlinux")
    , ("M-o M-t"      , addName "terminal"                        $ spawn myTerminal)
    , ("M-o M-S-T"    , addName "alt-terminal"                    $ spawn myAltTerminal)
    ] ^++^

    -----------------------------------------------------------------------
    -- ScratchPads
    -----------------------------------------------------------------------

    subKeys "scratchpads"
    [ 
      ("C-M1-<Delete>"          , addName "htop"                        $ namedScratchpadAction scratchpads "htop")
    , ("M-<F5>"                 , addName "weather"                     $ namedScratchpadAction scratchpads "weather")
    ] ^++^


    -----------------------------------------------------------------------
    -- Windows
    -----------------------------------------------------------------------

    subKeys "windows"
    (
    [ 
      ("M-<Backspace>"          , addName "kill"                            kill1)
    , ("M-C-<Backspace>"        , addName "kill all"                        $ confirmPrompt hotPromptTheme "kill all windows?" $ killAll)
    , ("M-w M-S-d"              , addName "duplicate w to all ws"           $ toggleCopyToAll)
    , ("M-w M-b"                , addName "hide window to stack"            $ withFocused hideWindow)
    , ("M-w M-S-b"              , addName "restore hidden window (FIFO)"    $ popOldestHiddenWindow)
    , ("M-w M-C-b"              , addName "restore hidden window (FILO)"    $ popNewestHiddenWindow)


    , ("M-w M-u"                , addName "un-merge from sublayout"         $ withFocused (sendMessage . UnMerge))
    --, ("M-u"                    , addName "un-merge from sublayout"         $ withFocused (sendMessage . UnMerge))
    , ("M-w S-m"                , addName "merge all into sublayout"        $ withFocused (sendMessage . MergeAll))
    --, ("M-w M--"                , addName "minimize"                        $ withWorkspace "minimized"  W.shift)

    , ("M-z u"                  , addName "focus urgent"                    focusUrgent)
    , ("M-g M-g"                , addName "focus master"                    $ windows W.focusMaster)
    , ("M-g M-p"                , addName "promote"                         $ promote) 
    , ("M-g M-m"                , addName "swap with master"                $ windows W.swapMaster)

    --, ("M-i"                    , addName "navigate tabs D/R"               $ bindOn LD [("tabs", windows W.focusDown), ("", onGroup W.focusDown')])
    --, ("M-u"                    , addName "navigate tabs U/L"               $ bindOn LD [("tabs", windows W.focusUp), ("", onGroup W.focusUp')])
    , ("M-S-i"                  , addName "swap tab D/R"                    $ windows W.swapDown)
    , ("M-S-u"                  , addName "swap tab U/L"                    $ windows W.swapUp)
    ]
    ++ zipM' "M-"               "navigate window"                           fulldirKeys fulldirs windowGo True
    ++ zipM' "M-S-"             "move window"                               fulldirKeys fulldirs windowSwap True
    ++ zipM  "M-C-"             "merge w/sublayout"                         fulldirKeys fulldirs (sendMessage . pullGroup)
    ++ zipM  "M-w M-"           "merge w/sublayout"                         fulldirKeys fulldirs (sendMessage . pullGroup)
    ++ zipM' "M-"               "navigate screen"                           screenKeys rstrdirs screenGo True
    ++ zipM' "M-S-"             "move window to screen"                     screenKeys rstrdirs windowToScreen True
    ++ zipM' "M-C-"             "Swap workspace to screen"                  screenKeys rstrdirs screenSwap True
    ) ^++^

    -----------------------------------------------------------------------
    -- Workspaces & Projects
    -----------------------------------------------------------------------

    subKeys "workspaces"
    (
    [ 
      ("M-n"                      , addName "next non-empty ws"             $ nextHidWS)
    , ("M-S-n"                    , addName "prev non-empty ws"             $ prevHidWS)
    , ("M-` M-`"                  , addName "Toggle last workspace"         $ toggleWS' ["NSP"])
    , ("M-' M-'"                  , addName "select workspace"              $ selectWorkspace myPromptTheme)
    , ("M-' M-m"                  , addName "move to workspace"             $ withWorkspace myPromptTheme (windows . W.shift))
    , ("M-' M-r"                  , addName "rename workspace"              $ renameWorkspace myPromptTheme)
    , ("M-' M-<Backspace>"        , addName "remove workspace"              $ confirmPrompt hotPromptTheme "delete workspace?" $ removeWorkspace)
    , ("M-' M-l"                  , addName "next non-empty ws"             $ nextHidWS)
    , ("M-' M-h"                  , addName "prev non-empty ws"             $ prevHidWS)
    ]
     ++ zipM "M-"                "view ws"                                wsKeys [0..] (withNthWorkspace W.greedyView)
     ++ zipM "M-S-"              "move w to ws"                           wsKeys [0..] (withNthWorkspace W.shift)
     ++ zipM "M-y M-"            "copy w to ws"                           wsKeys [0..] (withNthWorkspace copy)
    ) ^++^

    -- TODO: consider a submap for nav/move to specific workspaces based on first initial

    -----------------------------------------------------------------------
    -- Layouts & Sublayouts
    -----------------------------------------------------------------------

    subKeys "layout management"

    [ 
      ("M-<Tab>"                , addName "cycle all layouts"               $ sendMessage NextLayout)
    , ("M-S-<Tab>"              , addName "cycle sublayout"                 $ toSubl NextLayout)
    , ("M-C-<Tab>"              , addName "reset layout"                    $ setLayout $ XMonad.layoutHook conf)
    , ("M-t"                    , addName "toggle floating w"               $ withFocused toggleFloat)
    , ("M-S-f"                  , addName "tile all floating w"             $ sinkAll)

    , ("M-m M--"                    , addName "decrease master windows"         $ sendMessage (IncMasterN (-1)))
    , ("M-m M-S-="                  , addName "increase master windows"         $ sendMessage (IncMasterN 1))

    , ("M-r"                    , addName "reflect across Y axis / rotate on BSP"   $ tryMsgR (Rotate) (XMonad.Layout.MultiToggle.Toggle REFLECTX))
    --, ("M-S-r"                  , addName "force reflect (even on BSP)"     $ sendMessage (XMonad.Layout.MultiToggle.Toggle REFLECTX))
    , ("M-S-r"                  , addName "mirror"                          $ sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
    , ("M-<F11>"                , addName "toggle struts (i.e. panels)"     $ sendMessage ToggleStruts)
    , ("M-="                , addName "toggle struts (i.e. panels)"     $ sendMessage ToggleStruts)
    , ("M-S-<F11>"              , addName "fullscreen"                      $ sequence_ [ (withFocused $ windows . W.sink) 
                                                                            , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) ])
    , ("M-S-="              , addName "fullscreen"                      $ sequence_ [ (withFocused $ windows . W.sink) 
                                                                            , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) ])
    ] ^++^


    -----------------------------------------------------------------------
    -- Reference
    -----------------------------------------------------------------------
    -- recent windows not working
    -- , ("M4-<Tab>",              , addName "Cycle recent windows"        $ (cycleRecentWindows [xK_Super_L] xK_Tab xK_Tab))
    -- either not using these much or (in case of two tab items below), they conflict with other bindings
    -- so I'm just turning off this whole section for now. retaining for refernce after a couple months
    -- of working with my bindings to see if I want them back. TODO REVIEW
    --, ("M-s m"                  , addName "Swap master"                 $ windows W.shiftMaster)
    --, ("M-s p"                  , addName "Swap next"                   $ windows W.swapUp)
    --, ("M-s n"                  , addName "Swap prev"                   $ windows W.swapDown)
    --, ("M-<Tab>"                , addName "Cycle up"                    $ windows W.swapUp)
    --, ("M-S-<Tab>"              , addName "Cycle down"                  $ windows W.swapDown)

    -- sublayout specific (unused)
    -- , ("M4-C-S-m"               , addName "onGroup focusMaster"         $ onGroup focusMaster')
    -- , ("M4-C-S-]"               , addName "toSubl IncMasterN 1"         $ toSubl $ IncMasterN 1)
    -- , ("M4-C-S-["               , addName "toSubl IncMasterN -1"        $ toSubl $ IncMasterN (-1))
    -- , ("M4-C-S-<Return>"        , addName "onGroup swapMaster"          $ onGroup swapMaster')

    -----------------------------------------------------------------------
    -- Music
    -----------------------------------------------------------------------

    --subKeys "cmus"
    --[ 
      --("M-m b"                       , addName "next track"                  $ spawn "cmus -l 'b'")
    --, ("M-m z"                       , addName "previous track"              $ spawn "cmus -l 'z'")
    --, ("M-m c"                       , addName "play/pause"                  $ spawn "cmus -l 'c'")
    --, ("M-m s"                       , addName "toggle shuffle"              $ spawn "cmus -l 's'")
    --, ("M-m S-="                     , addName "volume +10%"                 $ spawn "cmus -l '+'")
    --, ("M-m -"                       , addName "volume -10%"                 $ spawn "cmus -l '-'")
    --] ^++^
     
    -----------------------------------------------------------------------
    -- Resizing
    -----------------------------------------------------------------------

    subKeys "resize"

    [

    -- following is a hacky hack hack
    --
    -- I want to be able to use the same resize bindings on both BinarySpacePartition and other
    -- less sophisticated layouts. BSP handles resizing in four directions (amazing!) but other
    -- layouts have less refined tastes and we're lucky if they just resize the master on a single
    -- axis.
    --
    -- To this end, I am using X.A.MessageFeedback to test for success on using the BSP resizing
    -- and, if it fails, defaulting to the standard (or the X.L.ResizableTile Mirror variants)
    -- Expand and Shrink commands.
      ("M-["                    , addName "expand (L on BSP)"           $ tryMsgR (ExpandTowards L) Shrink)
    , ("M-]"                    , addName "expand (R on BSP)"           $ tryMsgR (ExpandTowards R) Expand)
    , ("M-S-["                  , addName "expand (U on BSP)"           $ tryMsgR (ExpandTowards U) MirrorShrink)
    , ("M-S-]"                  , addName "expand (D on BSP)"           $ tryMsgR (ExpandTowards D) MirrorExpand)

    , ("M-C-["                  , addName "shrink (L on BSP)"           $ tryMsgR (ShrinkFrom R) Shrink)
    , ("M-C-]"                  , addName "shrink (R on BSP)"           $ tryMsgR (ShrinkFrom L) Expand)
    , ("M-C-S-["                , addName "shrink (U on BSP)"           $ tryMsgR (ShrinkFrom D) MirrorShrink)
    , ("M-C-S-]"                , addName "shrink (D on BSP)"           $ tryMsgR (ShrinkFrom U) MirrorExpand)

  --, ("M-r"                    , addName "Mirror (BSP rotate)"         $ tryMsgR (Rotate) (XMonad.Layout.MultiToggle.Toggle MIRROR))
  --, ("M-S-C-m"                , addName "Mirror (always)"             $ sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
  --, ("M4-r"                   , addName "BSP Rotate"                  $ sendMessage Rotate)

-- TODO: the following are potentially useful but I won't know till I work with BSP further
--    , ("M4-s"                   , addName "BSP Swap"                    $ sendMessage XMonad.Layout.BinarySpacePartition.Swap)
--    , ("M4-p"                   , addName "BSP Focus Parent"            $ sendMessage FocusParent)
--    , ("M4-n"                   , addName "BSP Select Node"             $ sendMessage SelectNode)
    --, ("M4-m"                   , addName "BSP Move Node"               $ sendMessage MoveNode)

    -- sublayout specific (unused)
    --  ("M4-C-S-."               , addName "toSubl Shrink"               $ toSubl Shrink)
    --, ("M4-C-S-,"               , addName "toSubl Expand"               $ toSubl Expand)
    ]
        where
            toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
                            [] -> windows copyToAll
                            _ -> killAllOtherCopies


    -----------------------------------------------------------------------
    -- Screens
    -----------------------------------------------------------------------
--    subKeys "Screens"
--    ([("M-C-<Right>", addName "Focus prev screen" prevScreen)
--    , ("M-C-<Left>" , addName "Focus next screen" nextScreen)
--    ]
--    ++ zipMod "Focus screen"                         screenKeys [0..] "M-"    (screenAction W.view)
--    ++ zipMod "Move client to screen"                screenKeys [0..] "M-S-"  (screenAction W.shift)
--    ++ zipMod "Swap workspace with screen"           screenKeys [0..] "M-M1-" (screenAction W.greedyView)
--    ++ zipMod "Swap workspace with and focus screen" screenKeys [0..] "M-C-"  (\s -> screenAction W.greedyView s >> screenAction W.view s)
--    ) ^++^

--    subKeys "Media Controls"
--    [
--    ("<XF86AudioMicMute>"      , addName "Mic Mute"                    $ spawn "notify-send mic mute")
--    ]
    

-- Mouse bindings: default actions bound to mouse events
-- Includes window snapping on move/resize using X.A.FloatSnap
-- Includes window w/h ratio constraint (square) using X.H.ConstrainedResize
myMouseBindings (XConfig {XMonad.modMask = myModMask}) = M.fromList $

    [ ((myModMask,               button1) ,(\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicMove (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button1), (\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask,               button3), (\w -> focus w
      >> mouseResizeWindow w
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button3), (\w -> focus w
      >> Sqr.mouseResizeWindow w True
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster ))

--    , ((mySecondaryModMask,      button4), (\w -> focus w
--      >> prevNonEmptyWS))
--
--    , ((mySecondaryModMask,      button5), (\w -> focus w
--      >> nextNonEmptyWS))

    ]

------------------------------------------------------------------------}}}
-- Startup                                                              {{{
---------------------------------------------------------------------------

myStartupHook = do
    spawn "feh --bg-scale /home/stranger/Pictures/recycled_texture_background_by_sandeep_m-d6aeau9_PZ9chud.jpg"
    spawn (myTerminal ++ " -e htop")
    dynStatusBarStartup myBarCreator myBarDestroyer

quitXmonad :: X ()
quitXmonad = io (exitWith ExitSuccess)

rebuildXmonad :: X ()
rebuildXmonad = 
    spawn "xmonad --recompile && xmonad --restart"

restartXmonad :: X ()
restartXmonad = 
    spawn "xmoand --restart"

------------------------------------------------------------------------}}}
-- Log                                                                  {{{
---------------------------------------------------------------------------


myLogHook = do
    -- following block for copy windows marking
    copies <- wsContainingCopies
    let check ws | ws `elem` copies =
                   pad . xmobarColor yellow red . wrap "&" "&"  $ ws
                 | otherwise = pad ws

    -- LogHook for fading windows
    --fadeWindowsLogHook myFadeHook
    ewmhDesktopsLogHook
    
    -- LogHook for dynamic bars using pretty print
    multiPP myLogPP myLogPP
    

myLogPP = defaultPP
    { ppCurrent             = xmobarColor active "" . wrap "[" "]"
    , ppTitle               = xmobarColor active "" . shorten 50
    , ppVisible             = xmobarColor base0  "" . wrap "(" ")"
    , ppUrgent              = xmobarColor red    "" . wrap " " " "
    }
------------------------------------------------------------------------}}}
-- Actions                                                              {{{
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- Urgency Hook                                                            
---------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]
-- cf https://github.com/pjones/xmonadrc


---------------------------------------------------------------------------
-- New Window Actions
---------------------------------------------------------------------------

-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips#ManageHook_examples
-- <+> manageHook defaultConfig
--
myManageHook :: ManageHook
myManageHook =
        manageSpecific
    <+> manageOneSpecific
    <+> manageDocks
    <+> namedScratchpadManageHook scratchpads
    <+> fullscreenManageHook
    <+> manageSpawn
    where
        manageSpecific = composeAll . concat $ 
            -- composeOne . concat $
            [ 
              [ className =? c      --> doFloat       | c <- myFloatsC  ]
            , [ title     =? t      --> doFloat       | t <- myFloatsT  ]
            , [ className =? c      --> doCenterFloat | c <- myCenterFloatsC ]
            , [ title     =? t      --> doCenterFloat | t <- myCenterFloatsT ]
            , [ isDialog <&&> className =? b        --> forceCenterFloat   | b <- myWWWC ]
            -- note that counting workspaces starts at 0 instead of 1
            , [ className =? w      --> doShift "www" | w <- myWWWC   ]
            , [ className =? c      --> doShift "chat" | c <- myChatC  ]
            , [ className =? m      --> doShift "media" | m <- myMediaC ]
            , [ className =? m      --> doShift "mail" | m <- myMailC  ]
            , [ title     =? m      --> doShift "mail" | m <- myMailT  ]
            , [ className =? s      --> doShift "sys" | s <- mySysC   ]
            ]
        manageOneSpecific = composeOne
            [
              transience
            , resource   =? "desktop_window"            -?> doIgnore
            , className  =? "Tilda"                     -?> doFloat
            , isWinRole  =? "GtkFileChooserDialog"      -?> forceCenterFloat
            , isWinRole  =? "pop-up"                    -?> doCenterFloat
            , isSkipTask <&&> className =? "Skype"      -?> insertPosition Below Older
            , isSplash                  -?> doCenterFloat
            , isDialog                  -?> doCenterFloat
            , isFullscreen              -?> doFullFloat
            , pure True                 -?> insertPosition Below Newer 
            ]
        isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
        isSkipTask = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR"
        isWinRole  = stringProperty "WM_WINDOW_ROLE"
        myFloatsC = [
                      "Wrapper-1.0"
                    , "Wrapper-2.0"
                    ]
        myFloatsT = [
                      "Whisker Menu"
                    --, "Notes - Notes"
                    , "Add New Items"
                    ]
        myCenterFloatsC = [
                            "Xmessage"
                          ]
        myCenterFloatsT = [
                            "Application Finder"
                          , "Calculator"
                          , "Panel"
                          ]
        myWWWC = [
                   "qutebrowser"
                 , "firefox"
                 , "Vivaldi-stable"
                 , "google-chrome"
                 ]
        myMediaC = [
                     "vlc"
                   , "mpv"
                   ]
        myChatC = [
                    "Skype"
                  , "Pidgin"
                  , "Wire"
                  , "Discord"
                  ]
        myMailC = [
                    "Thunderbird"
                  ]
        myMailT = [
                    "alacritty-mutt"
                  ]
        mySysC = [
                   "VirtualBox"
                 , "VirtualBox Manager"
                 , "VirtualBox Machine"
                 , "Hardinfo"
                 , "GParted"
                 , "Deluge"
                 ]

---------------------------------------------------------------------------
-- X Event Actions
---------------------------------------------------------------------------

-- Not mine. TODO: understnad and edit this
myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> handleEventHook def
                <+> XMonad.Layout.Fullscreen.fullscreenEventHook
                -- Create a Status bar for each screen
                <+> XMonad.Hooks.DynamicBars.dynStatusBarEventHook myBarCreator myBarDestroyer


-- BarCreator and Destroyer for dynamic bars
myBarCreator :: XMonad.Hooks.DynamicBars.DynamicStatusBar
myBarCreator (XMonad.S sid) = do
    t <- XMonad.liftIO Data.Time.LocalTime.getZonedTime
    XMonad.trace (show t ++ ": XMonad myBarCreator " ++ show sid)
    XMonad.Util.Run.spawnPipe ("xmobar --screen " ++ show sid ++ " ~/.xmonad/xmobar.conf")

myBarDestroyer :: XMonad.Hooks.DynamicBars.DynamicStatusBarCleanup
myBarDestroyer = do
    t <- XMonad.liftIO Data.Time.LocalTime.getZonedTime
    XMonad.trace (show t ++ ": XMonad myBarDestroyer")

---------------------------------------------------------------------------
-- Custom hook helpers
---------------------------------------------------------------------------

-- from:
-- https://github.com/pjones/xmonadrc/blob/master/src/XMonad/Local/Action.hs
--
-- Useful when a floating window requests stupid dimensions.  There
-- was a bug in Handbrake that would pop up the file dialog with
-- almost no height due to one of my rotated monitors.

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 1/2
    x = (1-w)/2
    y = (1-h)/2

-- I left this here because I want to explore using tags more
-- ... did I crib this from pjones config?
--
---- | If the given condition is 'True' then add the given tag name to
---- the window being mapped.  Always returns 'Nothing' to continue
---- processing other manage hooks.
--addTagAndContinue :: Query Bool -> String -> MaybeManageHook
--addTagAndContinue p tag = do
--  x <- p
--  when x (liftX . addTag tag =<< ask)
--  return Nothing

------------------------------------------------------------------------}}}

-- vim: ft=haskell:foldmethod=marker:foldlevel=4:expandtab:ts=4:sts=4:shiftwidth=4
