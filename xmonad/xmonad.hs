import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import System.Taffybar.Hooks.PagerHints (pagerHints)
import Graphics.X11.ExtraTypes.XF86
import Data.Monoid
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.SetWMName

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- colors
colorFocusedBorder :: String
colorFocusedBorder = "#8787af"

colorNormalBorder :: String
colorNormalBorder = "#202020"

-- define default config
baseConfig :: XConfig (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))
baseConfig = ewmh $ pagerHints desktopConfig

-- main
main :: IO ()
main = do
    spawn myBar
    xmonad myConfig

myModMask :: KeyMask
myModMask = mod4Mask

-- my config
myConfig :: XConfig (ModifiedLayout AvoidStruts (ModifiedLayout SmartBorder (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))))
myConfig = baseConfig
    { modMask = myModMask
    , terminal = myTerminal
    , startupHook = setWMName "LG3D"
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
myManageHook :: ManageHook
myManageHook = manageDocks <+> manageHookConfig <+> composeOne
    [ isFullscreen                -?> doF W.focusDown <+> doFullFloat
    , className =? "Skype"        -?> doFloat
    , className =? "Steam"        -?> doFloat
    , className =? "Shutter"      -?> doFloat
    , className =? "mpv"          -?> doFloat
    , className =? "feh"          -?> doFloat
    , className =? "File-roller"  -?> doFloat
    ]
    where manageHookConfig = manageHook baseConfig

-- layouts
myLayoutHook :: ModifiedLayout AvoidStruts (ModifiedLayout SmartBorder (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))) Window
myLayoutHook = avoidStruts $ smartBorders $ layoutHook baseConfig

-- event hook
myHandleEventHook :: Event -> X All
myHandleEventHook = docksEventHook

-- my keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig {XMonad.modMask = extraKeysModMask} = M.fromList
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
    , ((extraKeysModMask, xK_p          ), spawn myDmenu)

    -- lockscreen
    , ((extraKeysModMask, xK_Escape     ), spawn myLockScreen)

    -- restart xmonad
    , ((extraKeysModMask, xK_q          ), spawn myRestartXmonad)
    ]

-- my bar
myBar :: String
myBar = unwords
    [ "for pid in `pgrep taffybar`; do kill $pid; done;"
    , "taffybar &"
    ]

-- dmenu
myDmenu :: String
myDmenu = unwords
    [ "dmenu_run"
    , "-i"
    , "-p \">>>\""
    , "-fn Noto-18"
    , "-nb \"#000\""
    , "-nf \"#fff\""
    , "-sb \"#4285F4\""
    , "-sf \"#fff\""
    ]

-- terminal
myTerminal :: String
myTerminal = unwords
    [ "urxvt"
    , "-e tmux"
    ]

-- lockscreen
myLockScreen :: String
myLockScreen = unwords
    [ "xautolock"
    , "-locknow"
    ]

-- restart xmonad
myRestartXmonad :: String
myRestartXmonad = unwords
    [ "xmonad --recompile;"
    , "xmonad --restart;"
    , "notify-send 'Xmonad reloaded';"
    ]

-- border width
myBorderWidth :: Dimension
myBorderWidth = 1

-- workspaces
myWorkspaces :: [String]
myWorkspaces = map show ([1..9] :: [Int])

