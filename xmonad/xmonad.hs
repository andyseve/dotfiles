-- Author: Anish Sevekari
-- Last Modified: Tue 28 Feb 2023 08:23:23 AM EST
-- Based on : https://github.com/altercation/dotfiles-tilingwm
-- Based on : https://github.com/NeshHari/XMonad

-- TODO                                                                     {{{
-------------------------------------------------------------------------------
{-
\* change config to use polybar
\* need eww to do anything fancy with the bar
\* simple  bar with eww for  actions on click -- best options to go
\* If I need to use eww anyway, then I might as well use the simplest bar - xmobar
\* Polybar has better action control than xmobar -- not really.
\* Taffybar supports better things, but significantly harder and annoying to use with nixos
\* Bite the bullet and start using Taffybar.
\* Taffybar is also significantly larger
\* Taffybar also allows inbuilt eww widgets.
\* vlc moving with mouse between screens.
\* steam messages window popping up when opening steam.
-}
----------------------------------------------------------------------------}}}
-- Modules                                                                  {{{
-------------------------------------------------------------------------------
-- Core

import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Data.Function (on)
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Time.LocalTime
import System.Exit
import System.IO
-- Base
-- \||| will be imported through X.L.LayoutCombinators
import XMonad hiding ((|||))
-- Layout

-- Screen Functionalities
-- Workspace specific layouts
-- for modifying layout names
-- Resizable Horizontal Border

-- Smart space around windows
-- Layouts inside windows

-- Hooks

-- Managing docks

-- Actions
import XMonad.Actions.Commands
import XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GroupNavigation
import XMonad.Actions.Navigation2D
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Accordion
import XMonad.Layout.BorderResize
import XMonad.Layout.Decoration
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.StackSet (Stack (Stack), StackSet)
import XMonad.StackSet qualified as W
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
    . withNavigation2DConfig myNav2DConf
    . withUrgencyHook LibNotifyUrgencyHook
    . addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
    . ewmhFullscreen
    . ewmh
    . docks
    . dynamicSBs myBarSpawner
    $ myConfig

myConfig =
  def
    { borderWidth = baseBorder,
      clickJustFocuses = myClickJustFocuses,
      focusFollowsMouse = myFocusFollowMouse,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      manageHook = myManageHook,
      handleEventHook = myHandleEventHook,
      layoutHook = myLayoutHook,
      logHook = myLogHook,
      modMask = myModMask,
      startupHook = myStartupHook,
      terminal = myTerminal,
      workspaces = myWorkspaces
    }

----------------------------------------------------------------------------}}}
-- Workspaces                                                               {{{
-------------------------------------------------------------------------------

wsmain = "main"

wstex = "latex"

wscode = "code"

wsgame = "game"

wswww = "www"

wscom = "com"

wsmedia = "media"

wssys = "sys"

wsmin = "NSP"

myWorkspaces :: [String]
myWorkspaces = [wsmain, wswww, wstex, wscode, wsgame, wscom, wsmedia, wssys, wsmin]

----------------------------------------------------------------------------}}}
-- Applications                                                             {{{
-------------------------------------------------------------------------------

myTerminal = "kitty"

myBrowser = "firefox"

myAltBrowser = "google-chrome-stable"

myLauncher = "rofi -show run -modi drun,run"

myAltLauncher = "rofi -show drun -modi drun,run"

myKeyViewer = "rofi -i -dmenu -p 'Xmonad keys'"

myWinSearch = "rofi -show window -modi window,windowcd"

myFiles = "kitty -e ranger"

myEditor = "gvim"

myMusic = "ytmdesktop --no-sandbox | awk -F \"Listen: \" '{print \"<fc=#268bd2>ﱘ <fn=1>\"$2\"</fn></fc>\"; fflush();}' | tee /tmp/music_pipe"

myTrayPrimary = "sh ~/.xmonad/scripts/start_trayer.sh"

----------------------------------------------------------------------------}}}
-- Scratchpads                                                              {{{
-------------------------------------------------------------------------------

myScratchpads =
  [ NS "top" spawnTop findTop manageTop,
    NS "terminal" spawnTerminal findTerminal manageTerminal,
    NS "ranger" spawnRanger findRanger manageRanger,
    NS "khal" spawnKhal findKhal manageKhal,
    NS "music" spawnMusic findMusic manageMusic
  ]
  where
    centerFloating = customFloating $ W.RationalRect 0.25 0.25 0.5 0.5
    rightFloating = customFloating $ W.RationalRect 0.73 0.02 0.25 0.50

    spawnTop = myTerminal ++ " --class=top -e btop"
    findTop = resource =? "top"
    manageTop = centerFloating

    spawnWeather = myTerminal ++ "--class=weather -e weather"
    findWeather = resource =? "weather"
    manageWeather = centerFloating

    spawnTerminal = myTerminal ++ " --class=term"
    findTerminal = resource =? "term"
    manageTerminal = centerFloating

    spawnRanger = myTerminal ++ " --class=ranger -e ranger"
    findRanger = resource =? "ranger"
    manageRanger = centerFloating

    spawnKhal = myTerminal ++ " --class=khal -e ikhal"
    findKhal = resource =? "khal"
    manageKhal = rightFloating

    spawnMusic = myMusic
    findMusic = className =? "youtube-music-desktop-app"
    manageMusic = centerFloating

----------------------------------------------------------------------------}}}
-- Theme                                                                    {{{
-------------------------------------------------------------------------------

myFocusFollowMouse = False

myClickJustFocuses = False

-- solarized colors
base03 = "#002b36"

base02 = "#073642"

base01 = "#586e75"

base00 = "#657b83"

base0 = "#839496"

base1 = "#93a1a1"

base2 = "#eee8d5"

base3 = "#fdf6e3"

yellow = "#b58900"

orange = "#cb4b16"

red = "#dc322f"

magenta = "#d33682"

violet = "#6c71c4"

blue = "#268bd2"

cyan = "#2aa198"

green = "#859900"

white = "#FFFFFF"

black = "#000000"

myNormalBorderColor = base02

myFocusedBorderColor = active

active = green

activeWarn = red

inactive = base02

focuscolor = blue

unfocuscolor = base02

-- fonts

-- mySmallFont   = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
-- myFont        = mySmallFont
-- myBigFont     = mySmallFont
mySmallFont = "xft:Fira Code:style=Regular:size=6"

myFont = "xft:Fira Code:style=Regular:size=8"

myBigFont = "xft:Fira Code:style=Regular:size=10"

-- sizes
gap = 4

topbar = 4

toptitle = 22

baseBorder = 0

prompt = 20

status = 20

myBorder = Border {top = gap, bottom = gap, right = gap, left = gap}

-- this is a "fake title" used as a highlight bar in lieu of full borders
myTopBarTheme :: Theme
myTopBarTheme =
  def
    { fontName = myFont,
      inactiveBorderColor = base03,
      inactiveColor = base03,
      inactiveTextColor = base03,
      activeBorderColor = active,
      activeColor = active,
      activeTextColor = active,
      urgentBorderColor = red,
      urgentTextColor = yellow,
      decoHeight = topbar
    }

myTabTheme :: Theme
myTabTheme =
  def
    { fontName = myFont,
      activeColor = active,
      inactiveColor = base03,
      urgentColor = red,
      activeBorderColor = base03,
      inactiveBorderColor = base1,
      urgentBorderColor = base1,
      activeTextColor = base03,
      inactiveTextColor = base1,
      urgentTextColor = base1
    }

myPromptTheme =
  def
    { font = myFont,
      bgColor = base03,
      fgColor = active,
      fgHLight = base03,
      bgHLight = active,
      borderColor = base03,
      promptBorderWidth = 0,
      height = prompt,
      position = Top
    }

warmPromptTheme =
  myPromptTheme
    { bgColor = yellow,
      fgColor = base03,
      position = Top
    }

hotPromptTheme =
  myPromptTheme
    { bgColor = red,
      fgColor = base3,
      position = Top
    }

myShowWNameTheme =
  def
    { swn_font = myBigFont,
      swn_fade = 0.5,
      swn_bgcolor = "#000000",
      swn_color = "#FFFFFF"
    }

myButtonTheme :: Theme
myButtonTheme =
  defaultThemeWithImageButtons
    { fontName = myFont,
      activeColor = active,
      inactiveColor = base03,
      urgentColor = red,
      activeBorderColor = base03,
      inactiveBorderColor = base1,
      urgentBorderColor = base1,
      activeTextColor = base03,
      inactiveTextColor = base1,
      urgentTextColor = base1,
      decoHeight = toptitle,
      windowTitleIcons =
        [ (menuButton, CenterLeft 3),
          (closeButton, CenterRight 3),
          (maxiButton, CenterRight 18),
          (miniButton, CenterRight 33)
        ]
    }
  where
    -- Button definitions                                          --{{{
    menuButton' :: [[Int]]
    menuButton' =
      [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 1, 1, 0, 0, 1, 1, 0, 0],
        [0, 0, 1, 0, 0, 0, 0, 1, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 0, 0, 0, 0, 1, 0, 0],
        [0, 0, 1, 1, 0, 0, 1, 1, 0, 0],
        [0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      ]
    miniButton' :: [[Int]]
    miniButton' =
      [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 1, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 1, 1, 1, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      ]
    maxiButton' :: [[Int]]
    maxiButton' =
      [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 1, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 0, 0, 0, 0, 1, 1, 0],
        [0, 1, 1, 0, 0, 0, 0, 1, 1, 0],
        [0, 1, 1, 0, 0, 0, 0, 1, 1, 0],
        [0, 1, 1, 0, 0, 0, 0, 1, 1, 0],
        [0, 1, 1, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 1, 1, 1, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      ]
    closeButton' :: [[Int]]
    closeButton' =
      [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 1, 0, 0, 0, 0, 1, 1, 0],
        [0, 1, 1, 1, 0, 0, 1, 1, 1, 0],
        [0, 0, 1, 1, 1, 1, 1, 1, 0, 0],
        [0, 0, 0, 1, 1, 1, 1, 0, 0, 0],
        [0, 0, 0, 1, 1, 1, 1, 0, 0, 0],
        [0, 0, 1, 1, 1, 1, 1, 1, 0, 0],
        [0, 1, 1, 1, 0, 0, 1, 1, 1, 0],
        [0, 1, 1, 0, 0, 0, 0, 1, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      ]
    convertToBool :: [Int] -> [Bool]
    convertToBool = map (\x -> x == 1)
    menuButton = map convertToBool menuButton'
    miniButton = map convertToBool miniButton'
    maxiButton = map convertToBool maxiButton'
    closeButton = map convertToBool closeButton'

--------------------------------------------------------------------}}}

----------------------------------------------------------------------------}}}
-- Layouts                                                                  {{{
-------------------------------------------------------------------------------
myLayoutHook =
  showWorkspaceName $
    fullScreenToggle $
      refocusLastLayoutHook $
        onWorkspace wsgame (Full ||| tabs) $
          onWorkspace wsmedia (Full ||| float) $
            onWorkspace wscom (tabs_tall ||| float) $
              onWorkspace wswww (tabs_tall ||| tall) $
                (tall ||| tripane)
  where
    showWorkspaceName = showWName' myShowWNameTheme

    fullScreenToggle = mkToggle (single FULL)
    addTopBar = noFrillsDeco shrinkText myTopBarTheme

    mySpacing = spacingRaw True myBorder False myBorder True

    named n = renamed [(XMonad.Layout.Renamed.Replace n)]
    suffixed n = renamed [(XMonad.Layout.Renamed.AppendWords n)]
    -----------------------------------------------------------------------
    -- Tabs Layout                                                       --
    -----------------------------------------------------------------------
    tabs =
      named "tabs" $
        avoidStruts $
          trackFloating $
            addTopBar $
              addTabs shrinkText myTabTheme $
                Simplest
    -- This is a combined layout to stack applications inside tabs.
    -- Specifically useful to handle zoom.
    -- Not going to have functions to move around: one tab is the main one
    tabs_tall =
      named "tabs" $
        avoidStruts $
          trackFloating $
            windowNavigation $
              addTabs shrinkText myTabTheme $
                subLayout [] (mySpacing $ ResizableTall 1 (1 / 20) (2 / 3) []) $
                  Simplest
    -----------------------------------------------------------------------
    -- Two Columns Layout                                                --
    -----------------------------------------------------------------------
    -- --------------------------------------
    -- \|                  |                 |
    -- \|                  |      Tabs       |
    -- \|                  |                 |
    -- \|      Master      | --------------- |
    -- \|                  |                 |
    -- \|                  |      Tabs       |
    -- \|                  |                 |
    -- \|                  |                 |
    -- --------------------------------------
    tall =
      named "tall" $
        avoidStruts
        -- Need windowNavigation to merge windows
        $
          windowNavigation $
            addTopBar $
              addTabs shrinkText myTabTheme $
                mySpacing $
                  subLayout [] (Accordion ||| Simplest) $
                    (suffixed "1/2" $ ResizableTall 1 (1 / 20) (1 / 2) [2, 1])
                      ||| (suffixed "2/3" $ ResizableTall 1 (1 / 20) (3 / 5) [])

    -----------------------------------------------------------------------
    -- Three Columns Layout                                              --
    -----------------------------------------------------------------------
    -- 3 Columns: master on left
    -- 3 Columns master in middle
    tripane =
      named "tri" $
        avoidStruts
        -- Need windowNavigation to merge windows
        $
          windowNavigation $
            addTopBar $
              addTabs shrinkText myTabTheme $
                mySpacing $
                  subLayout [] (Accordion ||| Simplest) $
                    (suffixed "mid" $ ThreeColMid 1 (3 / 100) (1 / 3))
                      ||| (suffixed "left" $ ThreeCol 1 (3 / 100) (1 / 2))

    -----------------------------------------------------------------------
    -- Three Columns Layout
    -----------------------------------------------------------------------
    float =
      named "float" $
        avoidStruts $
          imageButtonDeco shrinkText myButtonTheme $
            borderResize $
              simpleFloat

----------------------------------------------------------------------------}}}
-- Bindings                                                                 {{{
-------------------------------------------------------------------------------
myModMask = mod4Mask -- super key (win)

-- Navconf and Helper functions                                             {{{
myNav2DConf =
  def
    { defaultTiledNavigation = centerNavigation,
      floatNavigation = centerNavigation,
      screenNavigation = lineNavigation,
      layoutNavigation =
        [ ("Full", centerNavigation),
          ("tabs", lineNavigation)
        ],
      unmappedWindowRect =
        [ ("Full", singleWindowRect),
          ("tabs", fullScreenRect)
        ]
    }

-- workspace navigation functions -- https://github.com/altercation
-- any workspace but scratchpad
shiftAndView dir =
  findWorkspace getSortByIndex dir (ignoringWSs ["NSP"]) 1
    >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)

-- hidden, non-empty workspaces less scratchpad
shiftAndView' dir =
  findWorkspace getSortByIndex dir (hiddenWS :&: Not emptyWS :&: ignoringWSs ["NSP"]) 1
    >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)

nextHidWS =
  findWorkspace getSortByIndex Next (hiddenWS :&: ignoringWSs ["NSP"]) 1
    >>= \t -> windows . W.view $ t

prevHidWS =
  findWorkspace getSortByIndex Prev (hiddenWS :&: ignoringWSs ["NSP"]) 1
    >>= \t -> windows . W.view $ t

nextNonEmptyWS =
  findWorkspace getSortByIndex Next (hiddenWS :&: Not emptyWS :&: ignoringWSs ["NSP"]) 1
    >>= \t -> windows . W.view $ t

prevNonEmptyWS =
  findWorkspace getSortByIndex Prev (hiddenWS :&: Not emptyWS :&: ignoringWSs ["NSP"]) 1
    >>= \t -> windows . W.view $ t

-- toggle any workspace but scratchpad
myToggle =
  windows $
    W.view
      =<< W.tag
        . head
        . filter
          ((\x -> x /= wsmin && x /= "SP") . W.tag)
        . W.hidden

-- toggling between floating and non-floating
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else (W.float w (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)) s)
    )

-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "show keybindings" $ io $ do
  h <- spawnPipe myKeyViewer
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()

-- Modified BindOn function
data XCond = WS | LD

chooseAction :: XCond -> (String -> X ()) -> X ()
chooseAction Main.WS f = withWindowSet (f . W.currentTag)
chooseAction Main.LD f = withWindowSet (f . description . W.layout . W.workspace . W.current)

bindOn :: XCond -> [(String, X ())] -> X ()
bindOn xc bindings = chooseAction xc $ chooser
  where
    chooser xc = case find ((xc ==) . fst) bindings of
      Just (_, action) -> action
      Nothing -> case find (("" ==) . fst) bindings of
        Just (_, action) -> action
        Nothing -> return ()

-- Tabs navigatoin functions
myFocusUp = bindOn LD [("tabs", windows W.focusUp), ("", onGroup W.focusUp')]

myFocusDown = bindOn LD [("tabs", windows W.focusDown), ("", onGroup W.focusDown')]

-- Visible workspace query
-- https://github.com/xmonad/xmonad-contrib/issues/293
--
isOnVisibleWS :: Query Bool
isOnVisibleWS = do
  w <- ask
  ws <- liftX $ gets windowset
  let allVisible = concat $ (maybe [] W.integrate) . W.stack . W.workspace <$> W.current ws : W.visible ws
      visibleWs = w `elem` allVisible
  return $ visibleWs

classNameWS :: Query (String, Bool)
classNameWS = liftM2 (\x y -> (x, y)) className isOnVisibleWS

----------------------------------------------------------------------------}}}

myKeys conf =
  let subKeys str ks = subtitle str : mkNamedKeymap conf ks

      wsIndices = [1, 5, 2, 3, 4, 6, 7, 8, 0]
      wsKeys = map show $ wsIndices
      screenKeys = ["a", "s"]
      dirKeys = ["j", "k", "h", "l"]
      arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
      fulldirKeys = ["j", "<D>", "k", "<U>", "h", "<L>", "l", "<R>"]
      fulldirs = [D, D, U, U, L, L, R, R]
      dirs = [D, U, L, R]
      rstrdirs = [L, R]

      -- zip commands
      zipM m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
      zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as
   in ---------------------------------------------------------------------------
      -- System / Utilities                                                   {{{
      ---------------------------------------------------------------------------
      subKeys
        "system"
        [ ("M-x M-r", addName "restart XMonad" $ spawn "xmonad --restart"),
          ("M-x M-S-r", addName "rebuild & restart XMonad" $ spawn "xmonad --recompile && xmonad --restart"),
          ("M-x M-e", addName "edit xmonad.hs" $ spawn (myEditor ++ " ~/.xmonad/xmonad.hs")),
          ("M-x M-l", addName "lock screen" $ spawn "physlock"),
          ("M1-C-l", addName "lock screen" $ spawn "physlock"),
          ("M-x M-q", addName "Quit XMonad" $ confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))
        ]
        ^++^
        ------------------------------------------------------------------------}}}
        -- Actions                                                              {{{
        ---------------------------------------------------------------------------
        subKeys
          "actions"
          [ -- sound
            ("M-<Page_Up>", addName "volume +5%" $ spawn "~/.xmonad/scripts/volume_pipe.sh up"),
            ("M-<Page_Down>", addName "volume -5%" $ spawn "~/.xmonad/scripts/volume_pipe.sh down"),
            ("M-<End>", addName "mute/unmute" $ spawn "~/.xmonad/scripts/volume_pipe.sh toggle"),
            ("<XF86AudioRaiseVolume>", addName "volume +5%" $ spawn "~/.xmonad/scripts/volume_pipe.sh up"),
            ("<XF86AudioLowerVolume>", addName "volume -5%" $ spawn "~/.xmonad/scripts/volume_pipe.sh down"),
            ("<XF86AudioMute>", addName "mute/unmute" $ spawn "~/.xmonad/scripts/volume_pipe.sh toggle"),
            -- brightness
            ("<XF86MonBrightnessDown>", addName "brightness -5" $ spawn "light -U 5"),
            ("<XF86MonBrightnessUp>", addName "brightness +5" $ spawn "light -A 5"),
            ("M-<XF86MonBrightnessDown>", addName "brightness min" $ spawn "light -S 5"),
            ("M-<XF86MonBrightnessUp>", addName "brightness max" $ spawn "light -S 100"),
            -- screenshots
            ("<Print>", addName "screenshot window" $ spawn "scrot -u \"%Y-%m-%d-%r.jpg\" -e 'mv \"$f\" ~/Pictures/screenshots/.'"),
            ("M-<Print>", addName "screenshot fullscreen" $ spawn "scrot \"%Y-%m-%d-%r.jpg\" -e 'mv \"$f\" ~/Pictures/screenshots/.'"),
            ("M-C-<Print>", addName "screenshot region" $ spawn "sleep 0.5; scrot -s \"%Y-%m-%d-%r.jpg\" -e 'mv \"$f\" ~/Pictures/screenshots/.'") -- sleep 0.5 is to avoid keypress cancel
          ]
        ^++^
        ------------------------------------------------------------------------}}}
        -- Launchers                                                            {{{
        ---------------------------------------------------------------------------
        subKeys
          "launchers"
          [ ("M-p", addName "launcher" $ spawn myLauncher),
            ("M-S-p", addName "launcher" $ spawn myAltLauncher),
            ("M-/", addName "window search" $ spawn myWinSearch),
            ("M-S-<Return>", addName "terminal" $ nextMatchOrDo History (className =? "kitty") (spawn myTerminal)),
            ("M-<Return>", addName "terminal" $ spawn myTerminal),
            ("M1-C-t", addName "terminal" $ spawn myTerminal),
            ("M-\\", addName "browser" $ nextMatchOrDo Forward (className =? "Firefox") (spawn myBrowser)),
            ("M-S-\\", addName "browser" $ spawn myBrowser),
            ("M-z", addName "logout" $ spawn "rofi-session"),
            ("M-S-o", addName "launcher" $ spawn myAltLauncher),
            ("M-o M-o", addName "launcher" $ spawn myLauncher),
            ("M-o M-b", addName "browser" $ spawn myBrowser),
            ("M-o M-S-b", addName "alt-browser" $ spawn myAltBrowser),
            ("M-o M-f", addName "files" $ spawn myFiles),
            ("M-o M-t", addName "terminal" $ spawn myTerminal),
            ("M-o M-p", addName "passwords" $ spawn "rofi-pass")
          ]
        ^++^
        ------------------------------------------------------------------------}}}
        -- Settings Apps                                                        {{{
        ---------------------------------------------------------------------------
        subKeys
          "settings"
          [ ("M-z M-v", addName "volume" $ spawn "pavucontrol"),
            ("M-z M-w", addName "wifi" $ spawn "nm-applet"),
            ("M-z M-s", addName "ssh" $ spawn "rofi-ssh")
          ]
        ^++^
        ------------------------------------------------------------------------}}}
        -- Windows                                                              {{{
        ---------------------------------------------------------------------------
        subKeys
          "Windows"
          ( [ ("M-<Backspace>", addName "kill" $ kill),
              ("M-C-<Backspace>", addName "kill all" $ confirmPrompt hotPromptTheme "kill all windows?" $ killAll),
              ("M-g M-m", addName "Focus Master" $ windows W.focusMaster),
              ("M-g M-S-m", addName "Promote to Master" $ windows W.swapMaster <+> windows W.focusMaster),
              ("M-g M-n", addName "Focus Urgent" $ focusUrgent),
              ("M-g M-t", addName "toggle floating window" $ withFocused toggleFloat),
              ("M-g M-g", addName "Unmerge" $ withFocused (sendMessage . UnMerge)),
              ("M-g M-s", addName "merge all" $ withFocused (sendMessage . MergeAll)),
              ("M-u", addName "Navigate tabs U" $ myFocusUp),
              ("M-i", addName "Navigate tabs D" $ myFocusDown),
              ("M-S-u", addName "Switch tabs U" $ windows W.swapUp),
              ("M-S-i", addName "Switch tabs D" $ windows W.swapDown),
              ("M-C-u", addName "merge w/sublayout" $ withFocused (sendMessage . mergeDir W.focusDown')),
              ("M-C-i", addName "merge w/sublayout" $ withFocused (sendMessage . mergeDir W.focusUp')),
              ("M1-<Tab>", addName "cycle windows" $ nextMatch Forward isOnVisibleWS),
              ("M1-S-<Tab>", addName "cycle windows" $ nextMatch Backward isOnVisibleWS),
              ("M1-`", addName "cycle apps" $ nextMatchWithThis Forward classNameWS)
            ]
              ++ zipM' "M-" "navigate window" fulldirKeys fulldirs windowGo True
              ++ zipM' "M-S-" "move window" fulldirKeys fulldirs windowSwap True
              ++ zipM "M-C-" "merge w/sublayout" fulldirKeys fulldirs (sendMessage . pullGroup)
              ++ zipM' "M-" "navigate screen" screenKeys rstrdirs screenGo True
              ++ zipM' "M-S-" "move window to screen" screenKeys rstrdirs windowToScreen True
              ++ zipM' "M-C-" "Swap workspaces to screen" screenKeys rstrdirs screenSwap True
          )
        ^++^
        ------------------------------------------------------------------------}}}
        -- Workspaces                                                           {{{
        ---------------------------------------------------------------------------
        subKeys
          "workspaces"
          ( [ ("M-' M-n", addName "next non-empty workspace" $ nextHidWS),
              ("M-' M-p", addName "prev non-empty workspace" $ prevHidWS),
              ("M-' M-'", addName "select workspace" $ selectWorkspace myPromptTheme)
            ]
              ++ zipM "M-" "view workspace" wsKeys [0 ..] (withNthWorkspace W.greedyView)
              ++ zipM "M-S-" "move window to workspace" wsKeys [0 ..] (withNthWorkspace W.shift)
              ++ zipM "M-y M-" "copy window to workspace" wsKeys [0 ..] (withNthWorkspace copy)
          )
        ^++^
        ------------------------------------------------------------------------}}}
        -- Layouts and SubLayouts                                               {{{
        ---------------------------------------------------------------------------
        subKeys
          "layouts"
          [ ("M-<Tab>", addName "cycle all layouts" $ sendMessage NextLayout),
            ("M-S-<Tab>", addName "cycle sublayout" $ toSubl NextLayout),
            ("M-C-<Tab>", addName "reset layout" $ setLayout $ XMonad.layoutHook conf),
            ("M-S-t", addName "tile all floating windows" $ sinkAll),
            ( "M-S-=",
              addName "fullscreen" $
                sequence_
                  [ (withFocused $ windows . W.sink),
                    (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL)
                  ]
            ),
            ("M-' M-,", addName "Decrease master windows" $ sendMessage (IncMasterN (-1))),
            ("M-' M-.", addName "Increase master windows" $ sendMessage (IncMasterN 1)),
            ("M-' M-j", addName "Shrink master" $ sendMessage (Shrink)),
            ("M-' M-k", addName "Expand master" $ sendMessage (Expand))
          ]
        ^++^
        ------------------------------------------------------------------------}}}
        -- Scratchpads                                                          {{{
        ---------------------------------------------------------------------------
        subKeys
          "scratchpads"
          [ ("M-r", addName "top" $ namedScratchpadAction myScratchpads "top"),
            ("M-t", addName "terminal" $ namedScratchpadAction myScratchpads "terminal"),
            ("M-v", addName "mixer" $ namedScratchpadAction myScratchpads "mixer"),
            ("M-e", addName "ranger" $ namedScratchpadAction myScratchpads "ranger"),
            ("M-c", addName "khal" $ namedScratchpadAction myScratchpads "khal"),
            ("M-m", addName "music" $ namedScratchpadAction myScratchpads "music")
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
  spawn myTrayPrimary
  spawnOnce "slack"
  spawn "~/.xmonad/scripts/create_pipes.sh" -- this command needs to run at last

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
  historyHook
  fadeWindowsLogHook myFadeHook

-- FadeHook
myFadeHook =
  composeAll $
    [ opaque,
      isUnfocused --> opacity 0.95,
      isFloating --> opacity 0.95,
      isDialog --> opaque,
      isFullscreen --> opaque,
      isRole =? "browser" --> opaque -- Makes browser oqaque, important for videos
    ]
      ++ [className =? c --> opaque | c <- myOpaques]
  where
    isRole = stringProperty "WM_WINDOW_ROLE"
    myOpaques = ["vlc", "feh", "dota2", "Civ6Sub", "Terraria.bin.x86_64"]

----------------------------------------------------------------------------}}}
-- Xmobar                                                                   {{{
-------------------------------------------------------------------------------
myXmobarPP :: XMonad.Hooks.StatusBar.PP.PP
myXmobarPP =
  def
    { ppCurrent = xmobarColor blue "" . clickableWorkspaces,
      ppTitle = xmobarColor green "" . xmobarFont 1 . shorten 45,
      ppVisible = xmobarColor blue "" . clickableWorkspaces,
      ppUrgent = xmobarColor red "" . clickableWorkspaces,
      ppHidden = xmobarColor white "" . clickableWorkspaces,
      ppHiddenNoWindows = xmobarColor base01 "" . clickableWorkspaces,
      ppSep = "<fn=1> </fn>\xf101<fn=1> </fn>",
      ppWsSep = " ",
      ppLayout = xmobarColor yellow "" . xmobarFont 1 . shorten 5,
      ppSort = mkWsSort wsCompare
    }
  where
    fixIndex :: Maybe Int -> Int
    fixIndex Nothing = 9
    fixIndex (Just a) = a

    wsOrder :: [WorkspaceId]
    wsOrder = [wsmain, wstex, wscode, wsgame, wswww, wscom, wsmedia, wssys, wsmin]

    wsIndex :: WorkspaceId -> Int
    wsIndex "min" = 10
    wsIndex a = fixIndex $ elemIndex a wsOrder

    wsCompare :: X WorkspaceCompare
    wsCompare = return (compare `on` wsIndex)

    workspaceToIcons :: String -> String
    workspaceToIcons "main" = "\xf015" -- 
    workspaceToIcons "latex" = "\xf977" -- 亮
    workspaceToIcons "code" = "\xf673" -- 
    workspaceToIcons "game" = "\xf7b3" -- 󰊴\udb80\udeb4
    workspaceToIcons "www" = "\xf269" -- 
    workspaceToIcons "com" = "\xf679" -- 
    workspaceToIcons "media" = "\xfac2" -- 輸
    workspaceToIcons "sys" = "\xf120" -- 
    workspaceToIcons "NSP" = "\xfb12" -- ﬒
    workspaceToIcons _ = "\xf6a1" -- 
    clickableWorkspaces :: String -> String
    clickableWorkspaces ws = xmobarAction ("xdotool set_desktop " ++ show (fixIndex $ elemIndex ws myWorkspaces)) "1" (workspaceToIcons ws)
    -- clickableWorkspaces ws = "<action=xdotool set_desktop " ++ show ((wsIndex ws) + 1) ++">" ++ ws ++ "</action>"

    xmobarFont :: Int -> String -> String
    xmobarFont i ws = "<fn=" ++ show i ++ ">" ++ ws ++ "</fn>"

-- Looks too complicated to use xmobar action here

-- This is dumb, but literally anything else does not work.
myXmobar0Cmd = "xmobar -x 0 /home/stranger/.config/xmobar/xmobar.hs"

myXmobar1Cmd = "xmobar -x 1 /home/stranger/.config/xmobar/xmobar-secondary.hs"

myXmobar2Cmd = "xmobar -x 2 /home/stranger/.config/xmobar/xmobar-secondary.hs"

myXmobar0 = statusBarProp myXmobar0Cmd (pure myXmobarPP)

myXmobar1 = statusBarProp myXmobar1Cmd (pure myXmobarPP)

myXmobar2 = statusBarProp myXmobar2Cmd (pure myXmobarPP)

-- myXmobar :: ScreenId -> XMonad.Hooks.StatusBar.StatusBarConfig
-- myXmobar 0   = statusBarPropTo "_XMONAD_LOG" ("xmobar -x 0 /home/stanger/.config/xmobar/xmobar.hs") (pure myXmobarPP)
-- myXmobar sid = statusBarPropTo "_XMONAD_LOG" ("xmobar -x " ++ show sid ++ "/home/stranger/.config/xmobar/xmobar_secondary.hs") (pure myXmobarPP)

myBarSpawner :: ScreenId -> IO XMonad.Hooks.StatusBar.StatusBarConfig
myBarSpawner 0 = pure myXmobar0
myBarSpawner 1 = pure myXmobar1
myBarSpawner 2 = pure myXmobar2
myBarSpawner _ = mempty

----------------------------------------------------------------------------}}}
-- Manage Hook                                                              {{{
-------------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook =
  myCustomPlaceHook
    <+> myCustomShiftHook
    <+> myCustomStackHook
    <+> namedScratchpadManageHook myScratchpads -- Spawning and managing scratchpads
    <+> XMonad.Layout.Fullscreen.fullscreenManageHook
    <+> manageHook def

myCustomShiftHook :: ManageHook
myCustomShiftHook =
  composeOne . concat $
    [ [className =? c <||> resource =? c -?> doF (W.shift wswww) | c <- myWebShifts],
      [className =? c <||> resource =? c -?> doF (W.shift wsgame) | c <- myGameShifts],
      [className =? c <||> resource =? c -?> doF (shiftView wsgame) | c <- myGameViews],
      [className =? c <||> resource =? c -?> doF (W.shift wscom) | c <- myComShifts],
      [className =? c <||> resource =? c -?> doF (W.shift wsmedia) | c <- myMediaShifts],
      [className =? c <||> resource =? c -?> doF (shiftView wsmedia) | c <- myMediaViews]
    ]
  where
    myWebShifts = ["Firefox", "google-chrome"]
    myGameShifts = ["Steam"]
    myGameViews = ["dota2", "Civ6Sub", "Terraria.bin.x86_64"]
    myComShifts = ["Slack", "discord", "weechat", "zoom", "Mail"]
    myMediaViews = []
    myMediaShifts = ["vlc"]

    shiftView = liftM2 (.) W.greedyView W.shift

myCustomPlaceHook :: ManageHook
myCustomPlaceHook =
  (composeOne . concat)
    -- Handling specific conditions
    [ [transience],
      [isFullscreen -?> doFullFloat],
      [isDialog -?> doCenterFloat],
      [isRole =? "pop-up" -?> doCenterFloat],
      -- Handling special programs
      [className =? c <||> resource =? c -?> doCenterFloat | c <- myCFloats],
      [className =? c <||> resource =? c -?> doRRectFloat | c <- myRFloats],
      [className =? c <||> resource =? c -?> doFullFloat | c <- myFullFloats]
    ]
    <+> (composeOne . concat)
      [ [isDialog -?> doF W.swapUp],
        [isRole =? "pop-up" -?> doF W.swapUp]
      ]
  where
    isRole = stringProperty "WM_WINDOW_ROLE"

    myCFloats = ["feh"]
    myRFloats = ["ikhal", "pavucontrol"]
    myFullFloats = ["steam", "dota2", "Civ6Sub"]
    myViewers = ["Zathura", "evince"]

    doRRectFloat = doRectFloat (W.RationalRect 0.73 0 0.25 0.50)

myCustomStackHook :: ManageHook
myCustomStackHook =
  composeOne . concat $
    [ [className =? c -?> tileBelowNoFocus | c <- myViewers],
      [pure True -?> tileBelow]
    ]
  where
    tileBelowNoFocus = insertPosition Below Older
    tileBelow = insertPosition Below Newer

    myViewers = ["Zathura", "evince"]

----------------------------------------------------------------------------}}}
-- Event Hook                                                               {{{
-------------------------------------------------------------------------------

myHandleEventHook =
  myCustomEventHook
    <+> refocusLastWhen myRefocusPred
    -- <+> XMonad.Hooks.DynamicBars.dynStatusBarEventHook myBarCreator myBarDestroyer -- Create dynamic status bars
    <+> XMonad.Layout.Fullscreen.fullscreenEventHook
    <+> handleEventHook def

myCustomEventHook = mempty

myRefocusPred = refocusingIsActive

----------------------------------------------------------------------------}}}
-- Urgency Hook                                                             {{{
-------------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    Just idx <- fmap (W.findTag w) $ gets windowset
    let cmd = "notify-send -u critical -a Urgent \"Workspace " ++ idx ++ "\" \"" ++ (show name) ++ "\""
    spawn cmd

----------------------------------------------------------------------------}}}

-- vim: ft=haskell:foldmethod=marker:expandtab:ts=4:sts=4:shiftwidth=4
