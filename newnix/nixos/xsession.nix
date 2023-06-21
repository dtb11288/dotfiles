{ pkgs, config, ... }:

{
  imports =
    [
      ./nvidia-xps-15.nix
    ];

  environment.systemPackages = with pkgs; [
    rofi
    rofi-rbw
    pinentry-gnome
    polybar
    xmonad-log
    rxvt_unicode
    xss-lock
    xautolock
    xsel
    dunst
    libnotify
    pavucontrol
    blueman
    networkmanagerapplet
    networkmanager-openvpn
    xdg_utils
    slock
    cbatticon
    volumeicon
    parcellite
    xdotool
    playerctl
    feh

    # for libinput-gestures
    python3
  ];

  location = {
    provider = "geoclue2";
    latitude = 21.0;
    longitude = 105.0;
  };

  programs.light.enable = true;

  services.redshift = {
    enable = true;
    temperature = {
      day = 5000;
      night = 4000;
    };
  };

  i18n.inputMethod = {
    enabled = "fcitx5";
    fcitx5.addons = with pkgs; [
      fcitx5-unikey
    ];
  };

  services.picom.enable = true;
  services.dbus.packages = [ pkgs.gcr ];

  services.xserver = {
    dpi = 192;
    enable = true;
    layout = "us";
    # videoDrivers = [ "intel" ];

    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
      touchpad.disableWhileTyping = true;
    };

    windowManager.leftwm = {
      enable = true;
    };

    desktopManager = {
      xterm.enable = false;
    };

    displayManager = {
      defaultSession = "none+leftwm";
      lightdm = {
        enable = true;
      };
      autoLogin.enable = true;
      autoLogin.user = "binh";

      sessionCommands = with pkgs; ''
        ${xorg.xset}/bin/xset r rate 200 25
        ${xorg.xset}/bin/xset dpms 300
        ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
        ${xorg.xrdb}/bin/xrdb -merge "$HOME/.Xresources"
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
        ${flatpak}/bin/flatpak run com.synology.SynologyDrive &
      '';
    };
  };

  programs.slock.enable = true;

  environment.variables = {
    # XCURSOR_SIZE = "64";
    # XCURSOR_PATH = [
    #  "${config.system.path}/share/icons"
    #  "$HOME/.icons"
    #  "$HOME/.nix-profile/share/icons/"
    # ];
    # GTK_DATA_PREFIX = [
    #  "${config.system.path}"
    # ];
  };
}
