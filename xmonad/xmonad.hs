import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- colors
colorFocusedBorder = "#8787af"
colorNormalBorder = "#202020"
colorXmobarCurrent = "#87afd7"
colorXmobarTitle = "#ffaf5f"
colorXmobarUrgent = "red"
colorFallback = ""

-- define default config
baseConfig = desktopConfig

-- main
main = do
    xmtray <- spawnPipe myTrayBar
    xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- my config
myConfig = baseConfig
    { modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> manageHook baseConfig <+> myManageHook
    , layoutHook = avoidStruts $ layoutHook baseConfig
    , focusedBorderColor = colorFocusedBorder
    , normalBorderColor = colorNormalBorder
    , borderWidth = myBorderWidth
    } `additionalKeys` myKeys

myManageHook = composeOne [ isFullscreen -?> (doF W.focusDown <+> doFullFloat) ]

-- my PP
myPP = xmobarPP
    { ppCurrent = xmobarColor colorXmobarCurrent colorFallback . wrap "[" "]"
    , ppTitle = xmobarColor colorXmobarTitle colorFallback
    , ppUrgent = xmobarColor colorXmobarUrgent colorXmobarUrgent
    }

-- status bar
myBar = "xmobar ~/.xmonad/xmobar.hs"

-- keys
toggleStrutsKey XConfig { XMonad.modMask = myModMask } = (myModMask, xK_b)

-- use super key
myModMask = mod4Mask

-- terminal
myTerminal = "urxvt -e tmux"

-- workspaces
myWorkspaces = map show [1..9]

-- tray bar
myTrayBar = "killall -9 trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 8 --height 22 --transparent true --tint 0x000000"

-- border width
myBorderWidth = 1

-- my keys
myKeys =
    [
    -- volumn keys
      ((0, xF86XK_AudioLowerVolume   ), spawn "amixer -q sset Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer -q sset Master 5%+ unmute")
    , ((0, xF86XK_AudioMute          ), spawn "amixer -q sset Master toggle")

    -- brightness keys
    , ((0, xF86XK_MonBrightnessUp    ), spawn "xbacklight -inc 5 # increase screen brightness")
    , ((0, xF86XK_MonBrightnessDown  ), spawn "xbacklight -dec 5 # decrease screen brightness")
    ]
