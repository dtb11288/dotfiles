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
    spawnPipe myTrayBar
    xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- my config
myConfig = baseConfig
    { modMask = mod4Mask
    , terminal = myTerminal
    , focusFollowsMouse = False
    , workspaces = myWorkspaces
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , focusedBorderColor = colorFocusedBorder
    , normalBorderColor = colorNormalBorder
    , borderWidth = myBorderWidth
    , keys = \c -> myKeys c `M.union` keys defaultConfig c
    }

-- manage apps
myManageHook = manageDocks <+> manageHook baseConfig <+> composeOne
    [ isFullscreen -?> (doF W.focusDown <+> doFullFloat)
    , resource =? "trayer" -?> doIgnore
    ]

-- layouts
myLayoutHook = avoidStruts $ layoutHook baseConfig

-- my PP
myPP = xmobarPP
    { ppCurrent = xmobarColor colorXmobarCurrent colorFallback . wrap "[" "]"
    , ppTitle = xmobarColor colorXmobarTitle colorFallback . shorten 50
    , ppUrgent = xmobarColor colorXmobarUrgent colorXmobarUrgent
    }

-- my keys
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList
    -- volumn keys
    [ ((0, xF86XK_AudioLowerVolume  ), spawn "amixer -q sset Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume  ), spawn "amixer -q sset Master 5%+ unmute")
    , ((0, xF86XK_AudioMute         ), spawn "amixer -q sset Master toggle")

    -- brightness keys
    , ((0, xF86XK_MonBrightnessUp   ), spawn "xbacklight -inc 5 # increase screen brightness")
    , ((0, xF86XK_MonBrightnessDown ), spawn "xbacklight -dec 5 # decrease screen brightness")

    -- dmenu
    , ((modMask, xK_p               ), spawn myDmenu)
    ]

myDmenu = unwords
    [ "dmenu_run"
    , "-fn", "-*-roboto-medium-*-*-*-*-*-*-*-*-*-*-*"
    ]

-- show/hide top bar
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_f)

-- terminal
myTerminal = unwords
    [ "urxvt"
    , "-e", "tmux"
    ]

-- workspaces
myWorkspaces = map show [1..9]

-- tray bar
myTrayBar = unwords
    -- kill trayer
    [ "killall"
    , "-9", "trayer;"
    -- then re-run it
    , "trayer"
    , " --edge", "top"
    , "--align", "right"
    , "--SetDockType", "true"
    , "--SetPartialStrut", "true"
    , "--expand", "true"
    , "--width", "5"
    , "--height", "23"
    , "--transparent", "true"
    , "--alpha", "0"
    , "--tint", "0x080808"
    ]

-- status bar
myBar = unwords
    [ "xmobar"
    , "~/.xmonad/xmobar.hs"
    ]

-- border width
myBorderWidth = 1

