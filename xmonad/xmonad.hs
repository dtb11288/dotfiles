import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
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
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , focusedBorderColor = colorFocusedBorder
    , normalBorderColor = colorNormalBorder
    , borderWidth = myBorderWidth
    }

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
