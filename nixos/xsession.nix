{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    rofi
    polybar
    xmonad-log
    xorg.xbacklight
    rxvt_unicode
    alacritty
    xss-lock
    xautolock
    xsel
    dunst
    libnotify
    pavucontrol
    blueman
    networkmanagerapplet
    networkmanager_openvpn
    xdg_utils
    slock
    cbatticon
    volumeicon
    parcellite
    xdotool
    playerctl
    feh
    rclone

    # for libinput-gestures
    python3
  ];

  location = {
    latitude = 21.0;
    longitude = 105.0;
  };

  services.redshift = {
    enable = true;
    temperature = {
      day = 4100;
      night = 3100;
    };
  };

  i18n.inputMethod = {
    enabled = "fcitx";
    fcitx.engines = with pkgs.fcitx-engines; [ unikey ];
  };

  services.picom.enable = true;

  services.xserver = {
    dpi = 192;
    enable = true;
    layout = "us";
    videoDrivers = [ "intel" ];

    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
      touchpad.disableWhileTyping = true;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [
        hp.dbus
        hp.monad-logger
        hp.xmonad-contrib
      ];
    };

    desktopManager = {
      xterm.enable = false;
    };

    displayManager = {
      defaultSession = "none+xmonad";
      lightdm = {
        enable = true;
      };
      autoLogin.enable = true;
      autoLogin.user = "binh";

      sessionCommands = with pkgs; ''
        ${xorg.xset}/bin/xset r rate 200 25
        ${xorg.xset}/bin/xset dpms 300
        ${xorg.xrdb}/bin/xrdb -merge "$HOME/.Xresources"
        ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
        ${xss-lock}/bin/xss-lock -- "slock" &
        ${xautolock}/bin/xautolock -detectsleep -locker "slock" &
        ${feh}/bin/feh --bg-fill "$HOME/.wallpaper"
        ${networkmanagerapplet}/bin/nm-applet &
        ${volumeicon}/bin/volumeicon &
        ${parcellite}/bin/parcellite &
        ${cbatticon}/bin/cbatticon &
        ${blueman}/bin/blueman-applet &
        ${libinput-gestures}/bin/libinput-gestures &
        ${caffeine-ng}/bin/caffeine &
        ${rclone}/bin/rclone mount --daemon gdrive: "$HOME/gdrive" &
      '';
    };
  };

  security.wrappers.slock.source = "${pkgs.slock.out}/bin/slock";

  environment.variables = {
    # XCURSOR_SIZE = "64";
    # XCURSOR_PATH = [
    #  "${config.system.path}/share/icons"
    #  "$HOME/.icons"
    #  "$HOME/.nix-profile/share/icons/"
    # ];
    GTK_DATA_PREFIX = [
     "${config.system.path}"
    ];
  };
}
