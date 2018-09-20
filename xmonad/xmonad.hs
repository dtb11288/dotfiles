{-# LANGUAGE OverloadedStrings #-}
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import Graphics.X11.ExtraTypes.XF86
import Data.Monoid
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.SetWMName
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Hooks.DynamicLog
import qualified DBus as D
import qualified DBus.Client as D

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- colors
colorFocusedBorder :: String
colorFocusedBorder = "#8787af"

colorNormalBorder :: String
colorNormalBorder = "#202020"

-- define default config
baseConfig :: XConfig (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))
baseConfig = ewmh $ desktopConfig

-- main
main :: IO ()
main = do
    spawn myBar

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad myConfig { logHook = dynamicLogWithPP (myLogHook dbus) }

-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F#accaff} [ ") " ] %{F-}"
    -- , ppVisible = wrap ("%{B#fad07a} ") " %{B-}"
    , ppUrgent = wrap ("%{F#cf6a4c} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " : "
    , ppTitle = wrap ("%{F#99ad6a} ") " %{F-}" . shorten 120
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

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
myHandleEventHook = docksEventHook <+> minimizeEventHook <+> fullscreenEventHook

-- my keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig {XMonad.modMask = extraKeysModMask} = M.fromList
    -- volumn keys
    [ ((0, xF86XK_AudioLowerVolume      ), spawn "amixer -q sset Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume      ), spawn "amixer -q sset Master 5%+ unmute")
    , ((0, xF86XK_AudioMute             ), spawn "amixer -q sset Master toggle")

    -- media keys
    , ((0, xF86XK_AudioPrev             ), spawn "playerctl previous")
    , ((0, xF86XK_AudioNext             ), spawn "playerctl next")
    , ((0, xF86XK_AudioPlay             ), spawn "playerctl play-pause")

    -- brightness keys
    , ((0, xF86XK_MonBrightnessUp       ), spawn "light -A 5")
    , ((0, xF86XK_MonBrightnessDown     ), spawn "light -U 5")

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
    [ "for pid in `pgrep polybar`; do kill $pid; done;"
    , "polybar xmonad &"
    ]

-- dmenu
myDmenu :: String
myDmenu = unwords
    [ "dmenu_run"
    , "-i"
    , "-p \">>>\""
    , "-fn Noto-14"
    , "-nb \"#000\""
    , "-nf \"#fff\""
    , "-sb \"#668799\""
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
