{ pkgs, username, ... }:

{
  environment.systemPackages = with pkgs; [
    rofi
    rofi-rbw
    pinentry-gtk2
    polybar
    alacritty
    xss-lock
    xautolock
    xsel
    dunst
    libnotify
    glib
    pavucontrol
    blueman
    networkmanagerapplet
    networkmanager-openvpn
    xdg_utils
    slock
    volumeicon
    parcellite
    xdotool
    playerctl
    feh
    sxhkd
  ];

  location = {
    provider = "geoclue2";
    latitude = 21.0;
    longitude = 105.0;
  };

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

  services.xserver = {
    enable = true;
    layout = "us";

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
      autoLogin.user = username;

      sessionCommands = with pkgs; ''
        ${xorg.xset}/bin/xset r rate 200 25
        ${xorg.xset}/bin/xset dpms 300
        ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
        ${xss-lock}/bin/xss-lock -- "slock" &
        ${xautolock}/bin/xautolock -detectsleep -locker "slock" &
        ${networkmanagerapplet}/bin/nm-applet &
        ${volumeicon}/bin/volumeicon &
        ${parcellite}/bin/parcellite &
        ${blueman}/bin/blueman-applet &
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
