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
myConfig = baseConfig {
      modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , focusedBorderColor = colorFocusedBorder
    , normalBorderColor = colorNormalBorder
    , borderWidth = myBorderWidth
    } `additionalKeys` myKeys

-- manage apps
myManageHook = manageDocks <+> manageHook baseConfig <+> composeOne [
      isFullscreen -?> (doF W.focusDown <+> doFullFloat)
    , resource =? "trayer" -?> doIgnore
    ]

-- layouts
myLayoutHook = avoidStruts $ layoutHook baseConfig

-- my PP
myPP = xmobarPP {
      ppCurrent = xmobarColor colorXmobarCurrent colorFallback . wrap "[" "]"
    , ppTitle = xmobarColor colorXmobarTitle colorFallback
    , ppUrgent = xmobarColor colorXmobarUrgent colorXmobarUrgent
    }

-- my keys
myKeys = [
    -- volumn keys
      ((0, xF86XK_AudioLowerVolume   ), spawn "amixer -q sset Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer -q sset Master 5%+ unmute")
    , ((0, xF86XK_AudioMute          ), spawn "amixer -q sset Master toggle")

    -- brightness keys
    , ((0, xF86XK_MonBrightnessUp    ), spawn "xbacklight -inc 5 # increase screen brightness")
    , ((0, xF86XK_MonBrightnessDown  ), spawn "xbacklight -dec 5 # decrease screen brightness")
    ]

-- status bar
myBar = "xmobar ~/.xmonad/xmobar.hs"

-- show/hide top bar
toggleStrutsKey XConfig { XMonad.modMask = myModMask } = ( myModMask, xK_f )

-- use super key
myModMask = mod4Mask

-- terminal
myTerminal = "urxvt -e tmux"

-- workspaces
myWorkspaces = map show [1..9]

-- tray bar
myTrayBar = "killall -9 trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --height 23 --transparent true --alpha 0 --tint 0x080808"

-- border width
myBorderWidth = 1

