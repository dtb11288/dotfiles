Config
    { font = "xft:Roboto Mono for Powerline:size=12:antialias=true"
    , additionalFonts = ["xft:FontAwesome"]
    , border = FullBM 0
    , borderColor = "#151515"
    , fgColor = "#404040"
    , bgColor = "#080808"
    , position = Top
    , lowerOnStart = True
    , commands =
        [ Run Network "wlp2s0" ["-L","50","-H","500","--low","#4779b3","--normal","#4779b3","--high","#bf85cc","-t","<dev> <tx> Kb/s"] 8
        , Run Battery ["-t", "<left>"] 100
        , Run MultiCpu ["-t","<total0>"] 30
        , Run Date "%_d %#B %Y <fc=#AEB898>|</fc> %H:%M" "date" 600
        --, Run Com "/home/tzbob/bin/alsavolume" [] "volume" 10
        , Run StdinReader
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = " %StdinReader% }{<fc=#AEB898>net</fc> %wlp2s0% <fc=#AEB898>cpu</fc> %multicpu% <fc=#AEB898>bat</fc> %battery% <fc=#AEB898>|</fc> %date% "
    }
