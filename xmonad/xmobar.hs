Config
    { font = "xft:Roboto Mono for Powerline:size=12:antialias=true"
    , additionalFonts = ["xft:FontAwesome"]
    , border = FullBM 0
    , borderColor = "#151515"
    , fgColor = "#404040"
    , bgColor = "#080808"
    , position = TopW L 95
    , lowerOnStart = True
    , commands =
        -- weather monitor
        [ Run Weather "VVNB" [ "--template", "<skyCondition> - <fc=#4682B4><tempC></fc>°C"
                             ] 36000

        -- network activity monitor (dynamic interface resolution)
        , Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s-<rx>kB/s"
                             , "--Low"      , "1000"       -- units: kB/s
                             , "--High"     , "5000"       -- units: kB/s
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10

        -- cpu activity monitor
        , Run MultiCpu       [ "--template" , "Cpu: <total0>%"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10

         -- memory usage monitor
        , Run Memory         [ "--template" ,"Mem: <usedratio>%"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10

        -- battery monitor
        , Run Battery        [ "--template" , "Bat: <acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "darkred"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkgreen"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"   , "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"   , "<fc=#dAA520>Charging</fc>"
                                       -- charged status
                                       , "-i"   , "<fc=#006000>Charged</fc>"
                             ] 50
        -- time and date indicator
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run Date           "<fc=#ABABAB>%F (%a) %T</fc>" "date" 10
        --, Run Com "/home/tzbob/bin/alsavolume" [] "volume" 10
        , Run StdinReader
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = " %StdinReader% }{%VVNB% | %dynnetwork% | %memory% | %battery% | %date% |"
    }
