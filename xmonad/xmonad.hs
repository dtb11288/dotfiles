import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
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
baseConfig = ewmh desktopConfig

-- main
main = do
    spawnPipe myTrayBar
    xmonad =<< statusBar myBar myPP toggleXMobarKey myConfig

-- my config
myConfig = baseConfig
    { modMask = mod4Mask
    , terminal = myTerminal
    , focusFollowsMouse = False
    , handleEventHook    = myHandleEventHook
    , workspaces = myWorkspaces
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , focusedBorderColor = colorFocusedBorder
    , normalBorderColor = colorNormalBorder
    , borderWidth = myBorderWidth
    , keys = \c -> myKeys c `M.union` keys baseConfig c
    }

-- manage apps
myManageHook = manageDocks <+> manageHookConfig <+> composeOne
    [ isFullscreen                  -?> doF W.focusDown <+> doFullFloat
    , className =? "Skype"          -?> doFloat
    , className =? "Steam"          -?> doFloat
    , className =? "Shutter"        -?> doFloat
    , className =? "mpv"            -?> doFloat
    ]
    where manageHookConfig = manageHook baseConfig

-- layouts
myLayoutHook = avoidStruts $ smartBorders $ layoutHook baseConfig

-- event hook
myHandleEventHook = fullscreenEventHook

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

    -- media keys
    , ((0, xF86XK_AudioPrev         ), spawn "cmus-remote -r")
    , ((0, xF86XK_AudioNext         ), spawn "cmus-remote -n")
    , ((0, xF86XK_AudioPlay         ), spawn "cmus-remote -u")
    , ((0, xF86XK_AudioStop         ), spawn "cmus-remote -s")

    -- brightness keys
    , ((0, xF86XK_MonBrightnessUp   ), spawn "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown ), spawn "xbacklight -dec 5")

    -- display shutdown menu
    , ((modMask, xK_Delete          ), spawn myShutdownMenu)

    -- dmenu
    , ((modMask, xK_p               ), spawn myDmenu)
    , ((modMask .|. shiftMask, xK_p ), spawn myDesktopDmenu)

    -- restart xmonad
    , ((modMask, xK_q               ), spawn myRestartXmonad)
    ]

-- dmenu
myDmenu = unwords
    [ "dmenu_run"
    , "-fn", "-*-noto-medium-*-*-*-*-*-*-*-*-*-*-*"
    ]

-- i3 demnu desktop
myDesktopDmenu = unwords
    [ "i3-dmenu-desktop"
    , "--dmenu='"
    , "dmenu"
    , "-i"
    , "-fn", "-*-noto-medium-*-*-*-*-*-*-*-*-*-*-*"
    , "'"
    ]

-- shutdown menu
myShutdownMenu = unwords
    [ "xmobar"
    , "~/.xmonad/shutdown.hs"
    ]

-- terminal
myTerminal = unwords
    [ "urxvt"
    , "-e", "tmux"
    ]

-- tray bar
myTrayBar = unwords
    [ "trayer"
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

-- restart xmonad
myRestartXmonad = unwords
    [ "killall", "-9", "trayer;"
    , "xmonad", "--recompile;"
    , "xmonad" , "--restart;"
    , "notify-send", "'Xmonad reloaded';"
    ]

-- border width
myBorderWidth = 1

-- show/hide top bar
toggleXMobarKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

-- workspaces
myWorkspaces = map show [1..9]

