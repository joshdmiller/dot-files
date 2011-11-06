-- Xmonad configuration file
--
-- author: Josh Miler <josh@joshdmiller.com>
-- license: Creative Commons Attribution 3.0 Unported License.
--
-- tutorial: http://www.joshdmiller.com/post/xmonad-tutorial-201105
--
-- updated: 07 May 2011

-- Core Functionality
import Monad (liftM)
import System.IO
import Graphics.X11.Xlib
import System.Exit

import XMonad
import XMonad.Core
 
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

import qualified Data.Map as M
import qualified XMonad.StackSet as W
 
-- Included Layouts 
-- Any layouts you may want to include need to be added here
-- TODO: remove unused layouts
import XMonad.Layout
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed

import XMonad.Layout.ResizableTile -- (5)  resize non-master windows too
import XMonad.Layout.Grid          -- (6)  grid layout
import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders     -- (7)  get rid of borders sometimes
                                   -- (8)  navigate between windows
import XMonad.Layout.WindowNavigation  --  directionally
import XMonad.Layout.Named         -- (9)  rename some layouts
import XMonad.Layout.PerWorkspace  -- (10) use different layouts on different WSs
import XMonad.Layout.WorkspaceDir  -- (11) set working directory
                                   --      per-workspace
import XMonad.Layout.Reflect       -- (13) ability to reflect layouts
import XMonad.Layout.MultiToggle   -- (14) apply layout modifiers dynamically
import XMonad.Layout.MultiToggle.Instances
                                   -- (15) ability to magnify the focused window
import XMonad.Layout.Tabbed
import XMonad.Layout.DwmStyle
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.Gaps

-- Included Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.UpdatePointer
 

-- Included Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName

import XMonad.Layout.DragPane

-------------------------------------------------------------------------
-- PARAMETERS - IMPORTANT!!!
-------------------------------------------------------------------------

-- IMPORTANT!!!
-- TODO: You must configure the below. Without it, this Xmonad configuration
-- will NOT work as expected.

-- TODO: Set these to match your screen resolution
-- Calculations like toolbar widths and positions are made based on these.
-- This config has only been tested on 1680x1050 and 1280x768, but should work just
-- fine on all reasonably-sized resolutions. Smaller resolutions may need to 
-- get rid of some desktops or conky tools to avoid overflow.
screenWidth  = 1680
screenHeight = 1050

-- TODO: Set your username
username = "joshua"

-- TODO: Set your preferred commands
-- The command used to lock the screen, e.g. gnome-screensaver, xscreensaver,
-- slock, etc.
myScreenLock         = "/usr/bin/xscreensaver-command -lock"
-- Your preferred terminal, e.g. gnome-terminal, urxvt, xterm, lxterm, 
-- lilyterm, konsole, etc.
myTerminal           = "sakura"

-- The location of the image files. You need not set this if you downloaded
-- the package I provided and you do not have custom home folder. Just the
-- same, check it anyway.
myImagesPath = "/home/" ++ username ++ "/.xmonad/icons/"

-- The modifier mask
-- This is the key represented by "M" in the keybindings. mod4 is typically
-- the logo key and is my strong recommendation.
myModMask = mod4Mask
 
-------------------------------------------------------------------------
-- CUSTOM FUNCTIONS
-------------------------------------------------------------------------

-- These are functions I added (or, more probably stole from sample
-- configurations) to achieve some advanced functionality.

-- Determines if a window is visible
isVisible :: X (WindowSpace -> Bool)
isVisible = do
  vs <- gets (map (W.tag . W.workspace) . W.visible . windowset)
  return (\w -> (W.tag w) `elem` vs)

-- Load NetworkManager Applet (the same one used by GNOME)
myNetworkMonitor = "nm-applet"

-- This launches the xscreensaver daemon
-- If you usea screensaver as your lock, it needs to be running.
myScreensaver = "xscreensaver -no-splash"

-- Start ivman
-- ivman is an automounting daemon and it must be configured properly. I
-- followed https://wiki.archlinux.org/index.php/Ivman
myAutomounter = "ivman"

-- Restore the wallpaper
myWallpaper = "nitrogen --restore"

-------------------------------------------------------------------------
-- CUSTOMIZATIONS & APPEARANCE
-------------------------------------------------------------------------

-- These are the visual settings - the "theme", if you will. Change these
-- to suit your preference. The default theme is in dark grays. Background is
-- not set by Xmonad and is therefore not listed here. If you followed my tutorial,
-- background is set already.

-- Font
-- This is an Xorg font string. You can pick one using the graphical xfontsel tool. 
-- The Arch Linux package is called xorg-xfontsel.
myFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"

-- Colors
myBgBgColor = "black"             -- the screen bg color, assuming no wallpaper
myFgColor = "gray80"              -- the color for the top & bottom bar fonts, by default
myBgColor = "gray20"              -- the color of the top & bottom bars
myActiveBorderColor = "green"     -- the border color of the currently-active window
myInactiveBorderColor = "gray20"  -- the border color of all inactive windows
myTitleFgColor = "white"          -- the text color of the title of the current window

-- Launcher / Prompt
myHighlightedFgColor = "white"    -- the color of selected text
myHighlightedBgColor = "gray40"   -- the background color of selected text

-- Workspace Label: Currently Selected
myCurrentWsFgColor = "white"      -- the text color of the current workspace label
myCurrentWsBgColor = "gray40"     -- the bakcground color of the current workspace label

-- Workspace Label: Visible but not Focues (Xinerama)
myVisibleWsFgColor = "gray80"     -- the text color of visible workspaces' labels
myVisibleWsBgColor = "gray20"     -- the background color visible workspaces' labels

-- Workspace Label: Non-Selected with Windows
myHiddenWsFgColor = "gray80"      -- the text color

-- Workspace Label: Non-Selected without Windows
myHiddenEmptyWsFgColor = "gray50" -- the text color

-- Workspace Label: Needing Attention
myUrgencyHintFgColor = "white"    -- the text color
myUrgencyHintBgColor = "red"    -- the background color

-------------------------------------------------------------------------
-- TOP & BOTTOM BARS
-------------------------------------------------------------------------

-- In order to mange workspaces and layouts visually, xmonad can pipe information
-- to dzen. This section contains three dzen bars:
--   (1) The upper-right - conky pipes system stats to dzen2
--   (2) The upper-left - xmonad pipes workspace/layout/window information to dzen2
--   (3) The lower-left - conky pipes MPD information to dzen2.
-- This section also contains a task tray in the bottom right from the application
-- `trayer` and a program launcher called `XP` that is part of xmonad.

-- For the two dzen2 bars that get their information from conky, please see the two files
-- in the `bin` directory of my package: conky_bar for the top right and conky_bar_bottom
-- for the bottom left.

-- These are the options passed to all instances of Dzen and indicate colors and height.
myDzenGenOpts    = "-fg '" ++ myFgColor ++ "' -bg '" ++ myBgColor ++ "' -fn '" ++ myFont ++ "' -h '16'"
 
-- Xmonad Prompt config (the program launcher)
-- These settings simple position the launcher in the bottom and apply the 
-- style specified above. As configured, this will appear at the bottom of the
-- screen when the keybinding M-p is entered. See the keybindings section.
myXPConfig = defaultXPConfig {
  font = myFont,
  position = Bottom,
  promptBorderWidth = 0,
  height = 15,
  bgColor = myBgColor,
  fgColor = myFgColor,
  fgHLight = myHighlightedFgColor,
  bgHLight = myHighlightedBgColor
  }

-- UPPER-LEFT
-- Configuration for the dzen bar in the upper left that shows workspaces, the
-- icon representing the current layout, and the active window's title. The
-- ppLayout line translates layout names to icons. These will need to be changed 
-- if you add or remove layouts.
-- This configuration looks different than the dzen configurations to follow because
-- this information is passed to dzen2 by xmonad functions whereas we will instruct 
-- xmonad to spawn dzen2 for the other two bars.
myDzenPP h = defaultPP {
  ppOutput = hPutStrLn h,
  ppSep = "^bg(" ++ myBgBgColor ++ ")^r(1,15)^bg()",
  ppWsSep = "",
  ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor,
  ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor,
  ppHidden = wrapFg myHiddenWsFgColor,
  ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor,
  ppUrgent = wrapBg myUrgencyHintBgColor,
  ppTitle = (\x -> " " ++ wrapFg myTitleFgColor x),
  ppLayout  = dzenColor myFgColor"" .
                (\x -> case x of 
                    "ResizableTall" -> wrapBitmap "/tall.xbm"
                    "Mirror ResizableTall" -> wrapBitmap "/mtall.xbm"
                    "Full" -> wrapBitmap "/full.xbm"
                    "Tabbed Simplest" -> wrapBitmap "/tab.xbm"
                )
  }
  where
    wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
    wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
    wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
    wrapBitmap bitmap = "^p(5)^i(" ++ myImagesPath ++ bitmap ++ ")^p(5)"

-- UPPER-LEFT, CONT'D
-- A dzen bar to be established when there is a window needing attention on a
-- specific workspace.
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
      args = [
         "-x", "0", "-y", "576", "-h", "15", "-w", "1680",
         "-ta", "r",
         "-fg", "" ++ myUrgencyHintFgColor ++ "",
         "-bg", "" ++ myUrgencyHintBgColor ++ ""
         ]
    }

-- UPPER-LEFT, CONT'D
-- Command to load the status Bar in the upper-left
myStatusBar      = "dzen2 -w 1000  -ta l " ++ myDzenGenOpts
 
-- UPPER-RIGHT and BOTTOM-LEFT
-- Conky Bars
--     myConkyBar: the bar at the top right
--     myConkyBarBottom: the bar at the bottom left
myConkyBar       = "conky -a mr -c ~/.xmonad/bin/conky_bar | dzen2 -x 1000 -w 680 -ta r " ++ myDzenGenOpts
myConkyBarBottom = "conky -c ~/.xmonad/bin/conky_bar_bottom | dzen2 -y 1034 -x 0 -w 1680 -ta l " ++ myDzenGenOpts

-- BOTTOM-RIGHT
-- Trayer
-- This is the "system tray" in the bottom-right corner of your screen.
mySystemTray = "trayer --edge bottom --align right --height 16 --tint 0x333333"
  ++ " --SetDockType true --SetPartialStrut true --alpha 0 --transparent true"
  ++ " --width 300 --widthtype pixel"

-------------------------------------------------------------------------
-- LAYOUT DEFINITIONS
-------------------------------------------------------------------------

-- These properties apply to all layouts:
   -- avoidStruts: Automatically allows spacing for visible docks and bars
   -- smartBorders: Borders are not shown if there is only one window in the
   --   current workspace or if the window is floated and full-screen (e.g. mplayer).

-- Layouts Used:
   -- tiled: Windows are tiled vertically, the "master" window in the left
   --   column and all other windows in the right column, stacked vertically.
   -- Mirror tiled: Windows are tiled horizontally, the "master" window in
   --   the top row and all other windows in the bottom row, stacked horizontally.
   -- simpleTabbed: Active window is full-screen, but there are tabs at the
   --   top to switch windows
   -- Full: Active window is full-screen
myLayoutHook = avoidStruts $ smartBorders $ ( tiled ||| Mirror tiled ||| simpleTabbed ||| Full )
  where
    tiled = ResizableTall nmaster delta ratio []
    nmaster = 1
    delta = 3/100
    ratio = 1/2

-------------------------------------------------------------------------
-- WORKSPACE DEFINITIONS
-------------------------------------------------------------------------

-- These are the workspaces that will appear in the upper-right corner of the
-- screen. The format is "order name image", where order MUST match the order
-- provided; this could have been accomplished with a loop, but was less
-- aesthetically pleasing and less readable.

-- XDOTOOL is used to make these guys clickable; if it is not installed, they
-- will not be clickable. On click, the Mod4 + the number of the workspace is 
-- triggered, If the mod key is changed, this will need to be changed too. 

-- Dzen2 runs the workspaces list and the clickable tag
-- is relatively new, so you may need to build from source (e.g. the Arch package
-- os not recent enough, so dzen2-svn from the aur is required).

-- The images are expected to be found in the myImagesPath specified earlier in the
-- file and MUST be absolute as $HOME and ~ will not work with dzen2. Boo.

myWorkspaces = [
  supWsNum "1" "web" "fox.xbm",
  supWsNum "2" "dev" "arch_10x10.xbm",
  supWsNum "3" "term" "arch.xbm",
  supWsNum "4" "media" "note.xbm",
  supWsNum "5" "man" "info_03.xbm",
  supWsNum "6" "enc" "shroom.xbm",
  supWsNum "7" "misc" "empty.xbm",
  supWsNum "8" "misc" "full.xbm",
  supWsNum "9" "min" "pause.xbm"
  ]
  where
    supWsNum wsNum wsName wsImg = "^ca(1,xdotool key Super_L+" ++ wsNum ++ ") ^i(" ++ myImagesPath ++ wsImg ++ ") " ++ wsName ++ " ^ca()"

-- Workspace variables

-- You will need to use these to refer to your workspaces at other places in 
-- this file (e.g. for some custom rules in the next section). These are
-- zero-indexed! Workspace 1 is here as workspace 0, etc.

webWs    = (myWorkspaces !! 0)
devWs    = (myWorkspaces !! 1)
musicWs  = (myWorkspaces !! 2)
mediaWs  = (myWorkspaces !! 3)
manWs    = (myWorkspaces !! 4)
encWs    = (myWorkspaces !! 5)
misc1Ws   = (myWorkspaces !! 6)
misc2Ws   = (myWorkspaces !! 7)
minWs    = (myWorkspaces !! 8)

-------------------------------------------------------------------------
-- CUSTOM RULES
-------------------------------------------------------------------------

-- Rules for specific applications (e.g. to float by default)

-- These hooks fire when a window matching any of the rules below is opened
-- while in Xmonad. This allows you to customize how windows work by default.

-- The only rules I have here are to specify that GIMP, mplayer, and 
-- others should float by default. In addition, any window with a name
-- of "Copying files" (for nautilus, pcmanfm, and thunar) should also be
-- floated. There are, however, many other kinds of rules that can be specified,
-- such as always opening an application on a specific workspace or in a 
-- particular position.

-- Rules are set here by specifying the class of the window, a property set
-- by the application that the X server sends to the window manager along
-- with other window properties. As the window class was not distinguising
-- enough for the "copying files" dialogs (which should have been set by the
-- application as managed child windows to float automatically), I specified
-- the rule using a string property called "WM_NAME", which is literally the
-- name of the window.

-- You can determine the class name and other properties of a window with the
-- `xprop` tool. To use xprop, run xprop from a terminal emulator from within a
-- running X server. Your cursor should change to a crosshair; click on a
-- window to see the xprop application print out details like WM_CLASS (the
-- className used below) or WM_NAME. For any property labeled "(STRING)" in the
-- xprop info, you can add a stringProperty rule to these hooks (like the
-- "Copying Files" dialog below).

-- For more info on what kinds of rules you can set, see:
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Extending.html#15

myManageHook = composeAll
    [ 
        className                =? "Gimp"           --> doFloat 
    ,   className                =? "mplayer"        --> doFloat
    ,   className                =? "Truecrypt"      --> doFloat
    ,   className                =? "Mirage"         --> doFloat
    ,   className                =? "Ristretto"      --> doFloat
    ,   className                =? "Viewnior"       --> doFloat
    ,   className                =? "File-roller"    --> doFloat
    ,   stringProperty "WM_NAME" =? "Copying files"  --> doFloat
    ]
 
-------------------------------------------------------------------------
-- KEY BINDINGS
-------------------------------------------------------------------------

myKeys = \conf -> mkKeymap conf $
                [ 
                ---------------------------------------------------
                -- SPAWN PROGRAMS
                ---------------------------------------------------
                -- launch myTerminal, defined above
                  ("M-S-<Return>", spawn $ XMonad.terminal conf) 
                -- lock the screen
                , ("M-C-l",        spawn $ myScreenLock)
                -- kill the X server
                , ("M-C-<Esc>",    spawn $ "xkill")
                -- launch the Xmonad prompt / program launcher
                , ("M-p",          shellPrompt myXPConfig)
                
                ---------------------------------------------------
                -- MANIPULATE THE LAYOUT
                ---------------------------------------------------

                -- cycle forward through the defined layouts
                , ("M-<Space>",    sendMessage NextLayout)
                -- next window
                , ("M-<Tab>",      windows W.focusDown)
                -- previous window
                , ("M-S-<Tab>",    windows W.focusUp)
                -- focus the master window
                , ("M-m",          windows W.focusMaster)
                -- swap this window with the next one
                , ("M-S-k",        windows W.swapDown)
                -- swap this window with the previous one
                , ("M-S-j",        windows W.swapUp)
                -- turn on and off struts; this can be useful for full-screen
                -- programs that do not detect struts properly, like fillscreen
                -- firefox or chromium
                , ("M-b",          sendMessage ToggleStruts)
                -- decrease the size of the master area
                , ("M-h",          sendMessage Shrink)
                -- increase the size of the master area
                , ("M-l",          sendMessage Expand)
                -- decrease the size of this window (vert for tall, horiz for mirror)
                , ("M-S-h",        sendMessage MirrorShrink)
                -- increase the size of this window (vert for tall, horiz for mirror)
                , ("M-S-l",        sendMessage MirrorExpand)
                -- make a floating window no longer float
                , ("M-t",          withFocused $ windows . W.sink) 
                -- add one window to the master area
                , ("M-,",          sendMessage (IncMasterN 1))
                -- remove one window from the master area
                , ("M-.",          sendMessage (IncMasterN (-1)))
                -- swap current window with master or, if already master, 
                -- swap it with the next window
                , ("M-<Return>",   dwmpromote) 

                -----------------------------------------------------
                -- MISCELLANEOUS
                -----------------------------------------------------
                
                -- close the active window (like clicking the "x" in stacking WMs)
                , ("M-c",          kill)
                -- leave xmonad
                , ("M-S-q",        io (exitWith ExitSuccess))
                -- recompile and reload xmonad configuration; very handy for tweaking
                -- this file while logged in
                , ("M-q", spawn "killall conky dzen2; xmonad --recompile; xmonad --restart")

                -----------------------------------------------------
                -- WORKSPACES
                -----------------------------------------------------
                
                -- move active window to the next workspace
                , ("M-S-<Right>",  shiftToNext >> nextWS)
                -- move active window to the previous workspace
                , ("M-S-<Left>",   shiftToPrev >> prevWS) 
                -- go to the next workspace
                , ("M-<Left>",     prevNonEmptyWS )
                -- go to the previous workspace
                , ("M-<Right>",    nextNonEmptyWS )

                -----------------------------------------------------
                -- CUSTOM COMMANDS
                -----------------------------------------------------
                
                -- MPD controls (man mpc for details)
                -- pause if playing, play if paused/stopped
                , ("M-<Home>",          spawn "mpc toggle")
                -- advance one track
                , ("M-<Page_Up>",          spawn "mpc prev")
                -- go back one track
                , ("M-<Page_Down>",          spawn "mpc next")
                -- stop playback
                , ("M-<End>",          spawn "mpc stop")

                -- Volume Control
                , ("M-<Down>", spawn "amixer set \"Master Front\" 5%-")
                , ("M-<Up>", spawn "amixer set \"Master Front\" 5%+")
                ]

                -- makes M[1-9] switch workspaces and M-S[1-9] move a window to a workspace
                -- M-1 goes to workspace one, etc.
                -- M-S-1 moves active window to workspace one, etc.
                ++
                [ (m ++ i, windows $ f j)
                    | (i, j) <- zip (map show [1..9]) (XMonad.workspaces conf)
                    , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
                ]
    -- shortcuts to provide the next/prev workspaces
    where 
      nextNonEmptyWS = moveTo Next (WSIs (liftM (not .) isVisible))
      prevNonEmptyWS = moveTo Prev (WSIs (liftM (not .) isVisible))

--------------------------------------------------------------------------
-- THIS LAUNCHES XMONAD
--------------------------------------------------------------------------

-- This is the main event loop that launches xmonad and passes all of the
-- above configuration to xmonad. It also launches the appropriate dzen,
-- conky bars, and trayer.

main = do
   myStatusBarPipe <- spawnPipe myStatusBar      -- open the top bar
   conkyBar <- spawnPipe myConkyBar              -- open the top conky bar
   --systemTray <- spawnPipe mySystemTray
   conkyBarBottom <- spawnPipe myConkyBarBottom  -- open the bottom conky bar
   screensaver <- spawnPipe myScreensaver
   --automounter <- spawnPipe myAutomounter
   --networkMonitor <- spawnPipe myNetworkMonitor
   wallpaper <- spawnPipe myWallpaper
   numlock <- spawnPipe "numlockx"
   xsetroot <- spawnPipe "xsetroot -cursor_name left_ptr"
   xmonad $ myUrgencyHook $ defaultConfig        -- run xmonad
      { terminal = myTerminal
      , normalBorderColor  = myInactiveBorderColor
      , focusedBorderColor = myActiveBorderColor
      , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
      , layoutHook = myLayoutHook
      , startupHook = setWMName "XMONAD"
      , logHook = dynamicLogWithPP $ myDzenPP myStatusBarPipe
      , modMask = myModMask         -- the modifier key
      , keys = myKeys               -- keybindings
      , workspaces = myWorkspaces   -- workspaces
     }   


