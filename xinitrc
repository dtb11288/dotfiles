# run startup script
if [[ -r ~/.startup ]]; then
    ~/.startup
fi
exec dbus-launch leftwm
