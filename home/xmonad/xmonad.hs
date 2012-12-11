-- This is XMonad's primary configuration file and the entry point to all
-- further user-specific XMonad configuration.
--
-- This file has been tested on [Arch Linux][arch] and XMonad 0.10.
--
-- [arch]: http://www.archlinux.org

-- # IMPORTS

-- These first imports are language imports and do not impart specific
-- functionality for this configuration.
import Control.Monad (liftM)
import System.IO
import Graphics.X11.Xlib
import System.Exit
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Core
 
-- The XMonad prompt is the program launcher I use in XMonad. It is like
-- [dmenu][dmenu] but contains more features and is mnore easily themeable.
--
-- [dmenu]: http://tools.suckless.org/dmenu/
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man

-- I use the EZConfig XMonad configuration generator as I find the syntax more
-- manageable and intuitive. With some modifications, this configuration can be
-- used with the standard configuration process too.
import XMonad.Util.EZConfig

-- Run allows this configuration script to launch external applications. This
-- is used to launch programs via keybinding, to start some processes I like to
-- have running during my X session, and to process mouse clicks on the
-- workspace names in the top left bar.
import XMonad.Util.Run

-- These are the included layouts that can be employed later in the
-- configuration. Any layouts you may want to include need to be added here.

-- **TODO:** remove or comment out unused layouts

-- **TODO:** document all layouts

import XMonad.Layout
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile 
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.DragPane
import XMonad.Layout.Tabbed
import XMonad.Layout.DwmStyle
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.Gaps

-- These are the included actions that can be employed later in the
-- configuration.
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.UpdatePointer
import XMonad.Actions.GridSelect
import XMonad.Prompt.Window

-- These are the included hooks that can be employed later in the
-- configuration.
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName

-- # USER-SPECIFIC VARIABLES

-- You must configure the values below to match your environment prior to using
-- this configuration. Without it, this Xmonad configuration will NOT work as
-- expected. Calculations like toolbar widths and positions are made based on
-- these.  This config has only been tested on 1680x1050 and 1366x768, but
-- should work just fine on all reasonably-sized resolutions. Smaller
-- resolutions may need to get rid of some desktops or conky tools to avoid
-- overflow.
--
-- **TODO:** Use X11 libraries to generate these based on your screen resolution.
userScreenWidth  = "1680"
userScreenHeight = "1050"

-- The following are some width settings based on your defined screen height
-- and width. They are probably fine. but if you have a very large or very
-- small screen, you may need to adjust these.
workspacesWidth = "800"

-- For convenience, some of the above need to be converted by type or
-- calculated.
workspacesWidthInt = read workspacesWidth :: Int
userScreenWidthInt = read userScreenWidth :: Int
userScreenHeightInt = read userScreenHeight :: Int
userBarHeightInt = read userBarHeight :: Int

conkyBLX = 0
conkyBLY = userScreenHeightInt - userBarHeightInt
conkyBLW = quot userScreenWidthInt 2

conkyBRX = quot userScreenWidthInt 2
conkyBRY = userScreenHeightInt - userBarHeightInt
conkyBRW = quot userScreenWidthInt 2

-- This is the username of the person employing this configuration.
username = "joshua"

-- This is the sound control to pass to amixer when changing volume. This can
-- be Front Master, Master, and others depending on your sound card. Use
-- alsamixer to find which one you should use.
userAmixerControl = "Master"

-- The command used to lock the screen, e.g. gnome-screensaver, xscreensaver,
-- slock, etc.
userScreenlockCommand = "/usr/bin/slock"

-- This variable holds the user's preferred terminal, e.g. gnome-terminal,
-- urxvt, xterm, lxterm, lilyterm, konsole, etc.
userTerminalCommand = "urxvt"

-- The location of the image files. You need not set this if you downloaded
-- the package I provided and you do not have custom home folder. Just the
-- same, check it anyway.
userImagePath = "/home/" ++ username ++ "/.xmonad/icons/"

-- The modifier mask
-- This is the key represented by "M" in the keybindings. mod4 is typically
-- the logo key and is my strong recommendation.
userModMask = mod4Mask

-- This is the command used to launche the screensaver daemon If you use a
-- screensaver as your lock, it needs to be running for the screen locker
-- specified above to work.
-- userScreensaverDaemonCommand = "xscreensaver -no-splash"

-- This is the command used to restore the wallpaper on XMonad start. I use
-- [nitrogen][nitrogen].
--
-- [nitrogen]: http://projects.l3ib.org/nitrogen/
userWallpaperCommand = "nitrogen --restore"

-- # CUSTOM FUNCTIONS
--
-- These are functions I added (or, more probably stole from sample
-- configurations) to achieve some advanced functionality.

-- This function determines if a window is currently visible.
isVisible :: X (WindowSpace -> Bool)
isVisible = do
  vs <- gets (map (W.tag . W.workspace) . W.visible . windowset)
  return (\w -> (W.tag w) `elem` vs)

-- # APPEARANCE

-- These are the visual settings - the "theme", if you will. Change these
-- to suit your preference. The default theme is in dark grays. 
--
-- Background is not set by Xmonad and is therefore not listed here; if you
-- have a wallpaper restoring function specified above (e.g. nitrogen), that
-- application should have some mechanism for setting the wallpaper. Otherwise,
-- there are X11-specific commands that can be used more generically.

-- This is an Xorg font string. You can pick one using the graphical xfontsel
-- tool. Make sure the font is installed or there will be errors and your
-- XMonad will not look right.
userFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-iso8859-*"

-- This is the height used for all the dzen bars and the Prompt.
userBarHeight = "16"

-- These are color definitions used throughout the configuration. Each has its
-- own description.
myBgBgColor = "black"             -- the screen bg color, assuming no wallpaper
myFgColor = "gray80"              -- the color for the top & bottom bar fonts, by default
myBgColor = "gray20"              -- the color of the top & bottom bars
myActiveBorderColor = "green"     -- the border color of the currently-active window
myInactiveBorderColor = "gray20"  -- the border color of all inactive windows
myTitleFgColor = "white"          -- the text color of the title of the current window

-- These are colors used for the XMonad Prompt (i.e. the launcher).
myHighlightedFgColor = "white"    -- the color of selected text
myHighlightedBgColor = "gray40"   -- the background color of selected text

-- These are the colors used for the currently-selected workspace labels.
myCurrentWsFgColor = "white"      -- the text color of the current workspace label
myCurrentWsBgColor = "gray40"     -- the bakcground color of the current workspace label

-- These are the colors for the workspace label of visible but not focued (e.g.
-- Xinerama) workspaces.
myVisibleWsFgColor = "gray80"     -- the text color of visible workspaces' labels
myVisibleWsBgColor = "gray20"     -- the background color visible workspaces' labels

-- These are the colors for workspace labels that are not selected right now
-- but have windows.
myHiddenWsFgColor = "gray80"      -- the text color

-- These are the colors for the workspace labels that are not selected right
-- now and do not have windows.
myHiddenEmptyWsFgColor = "gray50" -- the text color

-- These are the colors for the workspace labels needing attention.
myUrgencyHintFgColor = "white"    -- the text color
myUrgencyHintBgColor = "red"    -- the background color

-- # TOP & BOTTOM BARS

-- In order to mange workspaces and layouts visually, XMonad can pipe information
-- to other applications. For my configuration, I use [dzen2][dzen] to draw the
-- top and bottom bars. In addition to piping XMonad information like
-- workspaces to dzen, I use [conky][conky] to pipe system information to a
-- dzen bar. This configuration contains three dzen bars:
-- 
-- 1. The top-right - conky pipes system stats to dzen2
-- 2. The top-left - xmonad pipes workspace/layout/window information to dzen2
-- 3. The bottom - conky pipes MPD information to dzen2.
--
-- This section also contains configuration information for a program launcher
-- called Prompt that is part of XMonad.
--
-- For the two dzen2 bars that get their information from conky, please see the files
-- in the `bin` directory of my package: conky_bar for the top right and conky_bar_bottom
-- for the bottom left. These two files contain the configuration for the conky
-- output that is piped to dzen in the top-right, bottom-right, and
-- bottom-left.
--
-- [conky]: http://conky.sourceforge.net/
-- [dzen]: https://sites.google.com/site/gotmor/dzen

-- ## General Dzen Options

-- First, the following options are passed to all instances of dzen we launch
-- here. They define the basic colors used as well as the height, which we want
-- to remain consistent to ensure the bars look like a top and bottom bar
-- instead of four separate bars.
myDzenGenOpts    = "-fg '" ++ myFgColor ++ "' -bg '" ++ myBgColor ++ "' -fn '" ++ userFont ++ "' -h '" ++ userBarHeight ++ "'"
 
-- ## Prompt (the program launcher)

-- These settings simply position the launcher at the bottom of the screen and
-- set its height and colors. As configured, this will appear at the bottom of
-- the screen when the keybinding M-p is entered. See the keybindings section.
myXPConfig = defaultXPConfig {
  position = Bottom,
  promptBorderWidth = 0,
  height = read userBarHeight,
  bgColor = myBgColor,
  fgColor = myFgColor,
  fgHLight = myHighlightedFgColor,
  bgHLight = myHighlightedBgColor,
  font = userFont
  }

-- ## Workspace Info Bar (Top Left)

-- This is the configuration for the dzen bar that appears in the top left that shows workspaces, the
-- icon representing the current layout, and the active window's title. This
-- configuration looks different than the dzen configurations to follow because
-- this information is passed to dzen2 by xmonad functions whereas we will
-- instruct XMonad to spawn dzen2 ourselves for the other two bars.
--
-- Each of the properties below specifies some part of the rendering.
-- ppCurrent, ppHidden/ppHiddenNoWindows, and ppVisible define functions for
-- generating the layout of current, hidden, and visible workspaces. ppUrgent
-- defines the layout for workspaces where a window has broadcast an urgent
-- message requiring attention. ppTitle defines a function for outputting the
-- title of the current window. ppSep is the separater used between workspaces,
-- the layout, and the window title.
myDzenPP h = defaultPP {
  ppOutput = hPutStrLn h,
  ppSep = "^bg(" ++ myBgBgColor ++ ")^r(1,"++ show(read(userBarHeight) - 1) ++")^bg()",
  ppWsSep = "",
  ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor,
  ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor,
  ppHidden = wrapFg myHiddenWsFgColor,
  ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor,
  ppUrgent = wrapBg myUrgencyHintBgColor,
  ppTitle = (\x -> " " ++ wrapFg myTitleFgColor x),

  -- ppLayout defines a function for outputting layout names. The method simply
  -- translates the name of a layout into a friendly icon that is drawn between
  -- the workspace list and the window title. These will need to be changed if
  -- you add or remove layouts or no icon will appear.  
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
    wrapBitmap bitmap = "^p(5)^i(" ++ userImagePath ++ bitmap ++ ")^p(5)"

-- This is the dzen command used to load the workspaces list.
myStatusBar      = "dzen2 -w " ++ workspacesWidth ++ "  -ta l " ++ myDzenGenOpts

-- ## An Urgency Bar

-- The urgency bar is a dzen bar to be established when there is a window
-- needing attention on a different workspace than the current one. Here, it is
-- positioned at the bottom of the screen.
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
      args = [
         "-x", "0",
         "-y", show( read (userScreenHeight) - read (userBarHeight) ),
         "-h", userBarHeight, 
         "-w", userScreenWidth,
         "-ta", "l",
         "-fg", "" ++ myUrgencyHintFgColor ++ "",
         "-bg", "" ++ myUrgencyHintBgColor ++ "",
         "-fn", userFont
         ]
    }

-- ## The Top Right Bar

-- This conky bar shows the current CPU load, RAM usage, core temperature, and
-- battery percentage.
myConkyBar       = "conky -a mr -c ~/.xmonad/bin/conky_bar | dzen2 -x " ++ workspacesWidth ++ " -w " ++ (show (userScreenWidthInt - workspacesWidthInt)) ++ " -ta r " ++ myDzenGenOpts

-- ## The Bottom Right Bar

-- This conky bar shows the current up and down network transfer rates, the
-- currently-connected WIFI access point, the temperature outside, and the date
-- and time.
myConkyBarBottomR = "conky -c ~/.xmonad/bin/conky_bar_bottom_right | dzen2 -y " ++ (show conkyBRY) ++ " -x " ++ (show conkyBRX) ++ " -w " ++ (show conkyBRW) ++ " -ta r " ++ myDzenGenOpts

-- ## The Bottom Left Bar

-- This conky bar shows the volume, MPD status, and currently-playing track.
myConkyBarBottomL = "conky -c ~/.xmonad/bin/conky_bar_bottom_left | dzen2 -y " ++ (show conkyBLY) ++ " -x " ++ (show conkyBLX) ++ " -w " ++ (show conkyBLW) ++ " -ta l " ++ myDzenGenOpts

-- # LAYOUT DEFINITIONS
--
-- These properties apply to all layouts:
--
-- * _avoidStruts_: Automatically allows spacing for visible docks and bars
-- * _smartBorders_: Borders are not shown if there is only one window in the
--   current workspace or if the window is floated and full-screen (e.g. mplayer).
--
-- The following layouts are used in this configuration:
-- 
-- * _tiled_: Windows are tiled vertically, the "master" window in the left
--   column and all other windows in the right column, stacked vertically.
-- * _Mirror tiled_: Windows are tiled horizontally, the "master" window in
--   the top row and all other windows in the bottom row, stacked horizontally.
-- * _simpleTabbed_: Active window is full-screen, but there are tabs at the
--   top to switch windows
-- * _Full_: Active window is full-screen

-- myLayoutHook is the variable that holds the list of layouts currently
-- employed in this configuration.
myLayoutHook = avoidStruts $ smartBorders $ ( tiled ||| Mirror tiled ||| simpleTabbed ||| Full )
  where
    tiled = ResizableTall nmaster delta ratio []
    nmaster = 1
    delta = 3/100
    ratio = 1/2

-- # WORKSPACES

-- These are the workspaces that will appear in the upper-left corner of the
-- screen. The format is "order name image", where order MUST match the order
-- provided; this could have been accomplished with a loop, but was less
-- aesthetically pleasing and less readable.
--
-- [Xdotool](http://www.semicomplete.com/projects/xdotool/) is used to make
-- these guys clickable; if it is not installed, they will not be clickable. On
-- click, the Mod4 + the number of the workspace is triggered, If the mod key
-- is changed, this will need to be changed too. 
--
-- Dzen2 runs the workspaces list and the clickable tag
-- is relatively new, so you may need to build from source (e.g. the Arch package
-- is not recent enough, so
-- [dzen2-svn](https://aur.archlinux.org/packages.php?ID=19926) from the AUR is
-- required).
--
-- The images are expected to be found in the userImagePath specified earlier in the
-- file and MUST be absolute as $HOME and ~ will not work with dzen2. Boo.

myWorkspaces = [
  supWsNum "1" "web" "fox.xbm",
  supWsNum "2" "dev" "arch_10x10.xbm",
  supWsNum "3" "term" "arch.xbm",
  supWsNum "4" "media" "play.xbm",
  supWsNum "5" "man" "info_03.xbm",
  supWsNum "6" "enc" "shroom.xbm",
  supWsNum "7" "misc" "empty.xbm",
  supWsNum "8" "misc" "empty.xbm",
  supWsNum "9" "min" "pause.xbm"
  ]
  where
    supWsNum wsNum wsName wsImg = "^ca(1,xdotool key Super_L+" ++ wsNum ++ ") ^i(" ++ userImagePath ++ wsImg ++ ") " ++ wsName ++ " ^ca()"

-- ## Workspace Variables

-- You will need to use these to refer to your workspaces at other places in 
-- this file (e.g. for some custom rules in the next section). These are
-- zero-indexed, unlike the list above; e.g. Workspace 1 is here as workspace
-- 0, etc.
webWs    = (myWorkspaces !! 0)
devWs    = (myWorkspaces !! 1)
musicWs  = (myWorkspaces !! 2)
mediaWs  = (myWorkspaces !! 3)
manWs    = (myWorkspaces !! 4)
encWs    = (myWorkspaces !! 5)
misc1Ws   = (myWorkspaces !! 6)
misc2Ws   = (myWorkspaces !! 7)
minWs    = (myWorkspaces !! 8)

-- # CUSTOM RULES

-- These are rules for specific applications (e.g. to float by default).
--
-- These hooks fire when a window matching any of the rules below is opened
-- while in Xmonad. This allows you to customize how windows work by default.
--
-- The only rules I have here are to specify that GIMP, mplayer, and 
-- others should float by default. In addition, any window with a name
-- of "Copying files" (for nautilus, pcmanfm, and thunar) should also be
-- floated. There are, however, many other kinds of rules that can be specified,
-- such as always opening an application on a specific workspace or in a 
-- particular position.
--
-- Rules are set here by specifying the class of the window, a property set
-- by the application that the X server sends to the window manager along
-- with other window properties. As the window class was not distinct
-- enough for the "copying files" dialogs (which should have been set by the
-- application as managed child windows to float automatically), I specified
-- the rule using a string property called "WM_NAME", which is literally the
-- name of the window.
--
-- You can determine the class name and other properties of a window with the
-- [xprop](http://www.xfree86.org/current/xprop.1.html) tool. To use xprop:
--
-- 1. run xprop from a terminal emulator from within a running X server. Your
-- cursor should change to a crosshair
-- 2. click on a window to see the xprop application print out details like
-- WM_CLASS (the className used below) or WM_NAME. 
--
-- For any property labeled "(STRING)" in the xprop info, you can add a
-- stringProperty rule to these hooks (like the "Copying Files" dialog below).
--
-- For more info on what kinds of rules you can set, see:
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Extending.html#15

myManageHook = composeAll
    [ 
        className                =? "Gimp"           --> doFloat 
    ,   className                =? "mplayer"        --> doFloat
    ,   className                =? "Truecrypt"      --> doFloat
    ,   className                =? "sxiv"           --> doFloat
    ,   className                =? "Synfig"         --> doFloat
    ,   className                =? "File-roller"    --> doFloat
    ,   stringProperty "WM_NAME" =? "Copying files"  --> doFloat
    ]
 
-- # KEY BINDINGS

-- There are several methods of specifying keybindings for XMonad. The
-- following way makes most sense to me; it uses string names for keys.
myKeys = \conf -> mkKeymap conf $
                [ 
                -- ## Spawning Programs
                
                -- Launch the userTerminalCommand defined above.
                  ("M-S-<Return>", spawn $ XMonad.terminal conf)
                   
                -- Lock the screen using the userScreenlockCommand defined
                -- above.
                , ("M-C-l",        spawn $ userScreenlockCommand)
                
                -- Kill the X server. Hopefully you don't need this.
                , ("M-C-<Esc>",    spawn $ "xkill")
                
                -- Launch the Xmonad prompt (the program launcher).
                , ("M-p",          shellPrompt myXPConfig)
                
                -- ## Manipulate the Layout

                -- Cycle forward through the defined layouts, rearranging the
                -- windows of the current workspace as we go.
                , ("M-<Space>",    sendMessage NextLayout)
                
                -- Advance to the next window.
                , ("M-<Tab>",      windows W.focusDown)
                
                -- Withdraw to the previous window.
                , ("M-S-<Tab>",    windows W.focusUp)
                
                -- Change window focus to the master window.
                , ("M-m",          windows W.focusMaster)
                
                -- Swap this window with the next one.
                , ("M-S-k",        windows W.swapDown)
                
                -- Swap this window with the previous one.
                , ("M-S-j",        windows W.swapUp)

                -- Raise this window to the top of the stack.
                , ("M-r",          windows W.shiftMaster)
                
                -- Toggle struts; this can be useful for full-screen programs
                -- that do not detect struts properly, like fullscreen firefox
                -- or chromium.
                , ("M-b",          sendMessage ToggleStruts)
                
                -- Decrease the size of the master area.
                , ("M-h",          sendMessage Shrink)
                
                -- Increase the size of the master area.
                , ("M-l",          sendMessage Expand)
                
                -- Decrease the size of this window (vert for tall, horiz for mirror).
                , ("M-S-h",        sendMessage MirrorShrink)
                
                -- Increase the size of this window (vert for tall, horiz for mirror).
                , ("M-S-l",        sendMessage MirrorExpand)
                
                -- Make a floating window no longer float.
                , ("M-t",          withFocused $ windows . W.sink) 
                
                -- Add one window to the master area.
                , ("M-,",          sendMessage (IncMasterN 1))
                
                -- Remove one window from the master area.
                , ("M-.",          sendMessage (IncMasterN (-1)))
                
                -- Swap current window with master or, if already master, 
                -- swap it with the next window.
                , ("M-<Return>",   dwmpromote) 

                -- ## Miscellaneous
                
                -- Close the active window (like clicking the "x" in stacking WMs).
                , ("M-c",          kill)

                -- Leave xmonad.
                , ("M-S-q",        io (exitWith ExitSuccess))
                
                -- Recompile and reload the XMonad configuration; this is very handy for tweaking
                -- this file while logged in.
                , ("M-q", spawn "killall conky dzen2; xmonad --recompile && xmonad --restart")
                
                -- Show all windows.
                , ("M-g", windowPromptGoto defaultXPConfig )
                
                -- ## MPD & Volume Commands
                
                -- Pause if playing and play if paused/stopped.
                , ("M-<Home>",          spawn "mpc toggle")

                -- Advance one track.
                , ("M-<Page_Up>",          spawn "mpc prev")
                
                -- Retreat one track.
                , ("M-<Page_Down>",          spawn "mpc next")
                
                -- Stop playback.
                , ("M-<End>",          spawn "mpc stop")

                -- Decrease the volume.
                , ("M-<Down>", spawn ("amixer set \""++ userAmixerControl ++"\" 5%-"))
                
                -- Increase the volume.
                , ("M-<Up>", spawn ("amixer set \""++ userAmixerControl ++"\" 5%+"))

                -- ## Workspaces
                
                -- Move the active window to the next workspace.
                , ("M-S-<Right>",  shiftToNext >> nextWS)

                -- Move the active window to the previous workspace.
                , ("M-S-<Left>",   shiftToPrev >> prevWS) 

                -- Go to the next workspace.
                , ("M-<Left>",     prevNonEmptyWS )

                -- Go to the previous workspace.
                , ("M-<Right>",    nextNonEmptyWS )
                ]

                -- ## Workspaces By Number
                -- 
                -- The following appends the list of commands above with
                -- keybindings to make M[1-9] switch to the specified workspace
                -- and M-S[1-9] move a window to the specified workspace.  For
                -- example, M-1 goes to workspace one and M-S-1 moves the
                -- active window to workspace one.
                ++
                [ (m ++ i, windows $ f j)
                    | (i, j) <- zip (map show [1..9]) (XMonad.workspaces conf)
                    , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
                ]

    -- These are just convenience methods for identifying the following and
    -- previous workspace.
    where 
      nextNonEmptyWS = moveTo Next (WSIs (liftM (not .) isVisible))
      prevNonEmptyWS = moveTo Prev (WSIs (liftM (not .) isVisible))

-- # LAUNCH XMONAD

-- This is the main event loop that launches XMonad and passes in all of the
-- above configuration. It also launches the ppropriate dzen, conky bars, and
-- other applications defined above.
--
-- Optionally, the following can also be included, should they have been
-- defined:
--
--     systemTray <- spawnPipe mySystemTray
--     screensaver <- spawnPipe userScreensaverDaemonCommand
main = do
   myStatusBarPipe <- spawnPipe myStatusBar        -- open the top bar
   conkyBar <- spawnPipe myConkyBar                -- open the top conky bar
   conkyBarBottomL <- spawnPipe myConkyBarBottomL  -- open the bottom conky bar
   conkyBarBottomR <- spawnPipe myConkyBarBottomR  -- open the bottom conky bar
   wallpaper <- spawnPipe userWallpaperCommand
--   screensaver <- spawnPipe userScreensaverCommand
   numlock <- spawnPipe "numlockx"
   xsetroot <- spawnPipe "xsetroot -cursor_name left_ptr"
--   xrdb <- spawnPipe "xrdb ~/.Xresources"          -- rxvt-unicode settings
--   urxtd <- spawnPipe "urxvtd"                     -- rxvt-unicode daemon
   xmonad $ myUrgencyHook $ defaultConfig          -- run xmonad
      { terminal = userTerminalCommand
      , normalBorderColor  = myInactiveBorderColor
      , focusedBorderColor = myActiveBorderColor
      , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
      , layoutHook = myLayoutHook
      , startupHook = setWMName "XMONAD"
      , logHook = dynamicLogWithPP $ myDzenPP myStatusBarPipe
      , modMask = userModMask                      -- the modifier key
      , keys = myKeys                              -- keybindings
      , workspaces = myWorkspaces                  -- workspaces
     }   


