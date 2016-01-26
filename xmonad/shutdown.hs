Config
    { font = "xft:Droid Sans Mono-10"
    , additionalFonts = ["xft:FontAwesome-10"]
    , border = FullB
    , borderColor = "#8a8a8a"
    , borderWidth = 2
    , bgColor = "#3c3b37"
    , fgColor = "#ffffff"
    , lowerOnStart = False
    , position = Static { xpos = 660 , ypos = 490, width = 600, height = 100 }
    , template = "}<fn=1></fn>   \
                  \<action=`systemctl poweroff`> Shutdown </action>    \
                  \<action=`systemctl reboot`>Reboot</action>    \
                  \<action=`ps -o pid | sed '3q;d' |xargs kill && systemctl suspend`>\
                  \Suspend</action>    \
                  \<action=`ps -o pid | sed '3q;d' |xargs kill`>Exit</action>{"
    }

