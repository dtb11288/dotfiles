#exec startkde
# system tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype percent --width 6 --transparent true --tint 0x000000 --height 17 &

# screensaver
xscreensaver -no-splash &

# set wallpaper
feh --bg-fill $HOME/.feh/wallpaper

# set the cursoe as pointer, otherwise X will appear as cursor
xsetroot -cursor_name left_ptr

# set colors for xsystem
xrdb $HOME/.Xresources

# time to start, the master - the window manager xmonad
exec xmonad
