import System.Taffybar

import System.Taffybar.Pager
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.MPRIS

import System.Taffybar.Widgets.PollingBar()
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU

import Graphics.UI.Gtk.General.RcStyle (rcParseString)

memCallback :: IO [Double]
memCallback = do
    mi <- parseMeminfo
    return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
    (_userLoad, systemLoad, totalLoad) <- cpuLoad
    return [totalLoad, systemLoad]

main :: IO ()
main = do
    let memCfg = defaultGraphConfig
            { graphDataColors = [(1, 0, 0, 1)]
            , graphLabel = Just "mem"
            }
        cpuCfg = defaultGraphConfig
            { graphDataColors =
                [ (0, 1, 0, 1)
                , (1, 0, 1, 0.5)
                ]
            , graphLabel = Just "cpu"
            }
        clock = textClockNew Nothing "<span fgcolor='orange'>%Y %b %d, %a %H:%M</span>" 1
        pager = taffyPagerNew defaultPagerConfig
            { activeWindow = colorize "#92BA3F" "" . escape . shorten 100
            , activeWorkspace = colorize "#87afd7" "" . wrap "[" "]" . escape
            }
        note = notifyAreaNew defaultNotificationConfig
        wea = weatherNew (defaultWeatherConfig "VVNB") {weatherTemplate = "$tempC$°C"} 10
        mpris = mprisNew defaultMPRISConfig
        mem = pollingGraphNew memCfg 1 memCallback
        cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
        tray = systrayNew
        font = "Noto Mono 12"

    rcParseString $ unwords
        [ "style \"default\" {"
        , "  font_name = \"" ++ font ++ "\""
        , "}"
        ]

    defaultTaffybar defaultTaffybarConfig
        { startWidgets = [ pager, note ]
        , endWidgets = [ clock, tray, wea, mem, cpu, mpris ]
        }
