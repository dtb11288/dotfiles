import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps()
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.Taffybar.Hooks.PagerHints (pagerHints)
import Graphics.X11.ExtraTypes.XF86
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- colors
colorFocusedBorder = "#8787af"
colorNormalBorder = "#202020"
colorFallback = ""

-- define default config
baseConfig = ewmh $ pagerHints desktopConfig

-- main
main = do
    spawn myBar
    xmonad myConfig

-- my config
myConfig = baseConfig
    { modMask = mod4Mask
    , terminal = myTerminal
    , focusFollowsMouse = False
    , handleEventHook = myHandleEventHook
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
    [ isFullscreen                              -?> doF W.focusDown <+> doFullFloat
    , className =? "Skype"                      -?> doFloat
    , className =? "Steam"                      -?> doFloat
    , className =? "Shutter"                    -?> doFloat
    , className =? "mpv"                        -?> doFloat
    , className =? "feh"                        -?> doFloat
    , className =? "File-roller"                -?> doFloat
    ]
    where manageHookConfig = manageHook baseConfig

-- layouts
myLayoutHook = avoidStruts $ smartBorders $ layoutHook baseConfig

-- event hook
myHandleEventHook = fullscreenEventHook

-- my keys
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList
    -- volumn keys
    [ ((0, xF86XK_AudioLowerVolume      ), spawn "amixer -q sset Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume      ), spawn "amixer -q sset Master 5%+ unmute")
    , ((0, xF86XK_AudioMute             ), spawn "amixer -q sset Master toggle")

    -- media keys
    , ((0, xF86XK_AudioPrev             ), spawn "cmus-remote -r")
    , ((0, xF86XK_AudioNext             ), spawn "cmus-remote -n")
    , ((0, xF86XK_AudioPlay             ), spawn "cmus-remote -u")
    , ((0, xF86XK_AudioStop             ), spawn "cmus-remote -s")

    -- brightness keys
    , ((0, xF86XK_MonBrightnessUp       ), spawn "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown     ), spawn "xbacklight -dec 5")

    -- dmenu
    , ((modMask, xK_p                   ), spawn myDmenu)

    -- restart xmonad
    , ((modMask, xK_q                   ), spawn myRestartXmonad)
    ]

-- my bar
myBar = unwords
    [ "for pid in `pgrep taffybar`; do kill $pid; done;"
    , "taffybar &"
    ]

-- dmenu
myDmenu = unwords
    [ "dmenu_run"
    , "-i"
    , "-p \">>>\""
    , "-fn", "Noto-14"
    , "-nb \"#000\""
    , "-nf \"#fff\""
    , "-sb \"#4285F4\""
    , "-sf \"#fff\""
    ]

-- terminal
myTerminal = unwords
    [ "urxvt"
    , "-e", "tmux"
    ]

-- restart xmonad
myRestartXmonad = unwords
    [ "xmonad", "--recompile;"
    , "xmonad" , "--restart;"
    , "notify-send", "'Xmonad reloaded';"
    ]

-- border width
myBorderWidth = 1

-- workspaces
myWorkspaces = map show [1..9]

