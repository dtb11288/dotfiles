# run startup script
if [[ -r ~/.startup ]]; then
    ~/.startup
fi
exec dbus-launch xmonad
# exec dbus-launch /data/opt/bin/rwm
# exec dbus-launch leftwm
# exec dbus-launch qtile
