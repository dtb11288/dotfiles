import System.Taffybar

import System.Taffybar.Pager
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS

import System.Taffybar.Widgets.PollingBar()
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU
import System.Information.Network

import Graphics.UI.Gtk.General.RcStyle (rcParseString)
import Data.IORef

memCallback :: IO [Double]
memCallback = do
    mi <- parseMeminfo
    return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
    (_userLoad, systemLoad, totalLoad) <- cpuLoad
    return [totalLoad, systemLoad]

netCallback :: String -> IORef [Integer] -> Double -> IORef [Double] -> IO [Double]
netCallback interface sample interval maxNetwork = do
    maybeThisSample <- getNetInfo interface
    case maybeThisSample of
        Just thisSample -> do
            lastSample <- readIORef sample
            lastMaxNetwork <- readIORef maxNetwork
            writeIORef sample thisSample
            let deltas = map (max 0 . fromIntegral) $ zipWith (-) thisSample lastSample
                speed = map (/interval) deltas
                newMaxNetwork = zipWith max speed lastMaxNetwork
                speedRatio = zipWith (/) speed newMaxNetwork
            writeIORef maxNetwork newMaxNetwork
            return speedRatio
        _ -> do
            writeIORef maxNetwork [0, 0]
            return [0, 0]

myGraph :: GraphConfig
myGraph = defaultGraphConfig
    { graphDataStyles = repeat Line
    , graphPadding = 2
    , graphWidth = 48
    }

main :: IO ()
main = do
    sample <- newIORef [0, 0]
    maxNetwork <- newIORef [0, 0]
    let memCfg = myGraph
            { graphDataColors = [(1, 0, 0, 1)]
            , graphLabel = Just "mem"
            }
        netCfg = myGraph
            { graphDataColors =
                [ (0, 1, 0, 1)
                , (1, 0, 0, 1)
                ]
            , graphLabel = Just "net"
            }
        cpuCfg = myGraph
            { graphDataColors =
                [ (0, 1, 0, 1)
                , (1, 0, 1, 0.5)
                ]
            , graphLabel = Just "cpu"
            }
        clock = textClockNew Nothing "<span fgcolor='orange'>%a, %b %d, %Y - %H:%M</span>" 1
        pager = taffyPagerNew defaultPagerConfig
            { activeWindow = colorize "#92BA3F" "" . shorten 80 . escape
            , activeWorkspace = colorize "#87afd7" "" . wrap "[" "]" . escape
            }
        mpris = mprisNew defaultMPRISConfig
        mem = pollingGraphNew memCfg 1 memCallback
        cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
        tray = systrayNew
        net = pollingGraphNew netCfg 0.5 $ netCallback "wlp2s0" sample 0.5 maxNetwork

    rcParseString $ unwords
        [ "style \"default\" {"
        , "  font_name = \"Noto Mono 14\""
        , "}"
        ]

    defaultTaffybar defaultTaffybarConfig
        { startWidgets = [ pager ]
        , endWidgets = [ clock, tray, mem, cpu, net, mpris ]
        , barHeight = 24
        }
