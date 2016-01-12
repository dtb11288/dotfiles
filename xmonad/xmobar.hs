Config { font = "xft:Roboto Mono for Powerline:size=12:antialias=true"
       , additionalFonts = ["xft:FontAwesome"]
       , border = FullB
       , fgColor = "#DEE3E0"
       , bgColor = "#343C48"
       , position = Static { xpos = 0, ypos = 0, width = 1920, height = 24 }
       , lowerOnStart = True
       , commands = [ Run Battery ["-t", "<left>"] 100
                    , Run MultiCpu ["-t","<total0>"] 30
                    , Run Date "%_d %#B %Y <fc=#AEB898>|</fc> %H:%M" "date" 600
                    --, Run Com "/home/tzbob/bin/alsavolume" [] "volume" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% }{<fc=#AEB898>cpu</fc> %multicpu% <fc=#AEB898>bat</fc> %battery% <fc=#AEB898>|</fc> %date% "
       }
