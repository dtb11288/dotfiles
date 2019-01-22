{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    dmenu
    xorg.xbacklight
    rxvt_unicode
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
    enpass
    python3
  ];

  services.redshift = {
    enable = true;
    latitude = "21";
    longitude = "105";
    temperature = {
      day = 4100;
      night = 3100;
    };
  };

  services.xserver = {
    dpi = 192;
    enable = true;
    layout = "us";
    videoDrivers = [ "intel" ];
    windowManager = {
      default = "qtile";
      qtile.enable = true;
    };

    libinput = {
      enable = true;
      naturalScrolling = true;
      disableWhileTyping = true;
    };

    displayManager.sessionCommands = ''
      ${pkgs.xorg.xset}/bin/xset r rate 200 25
      ${pkgs.xorg.xset}/bin/xset dpms 300
      ${pkgs.xorg.xrdb}/bin/xrdb -merge "$HOME/.Xresources"
      ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
      ${pkgs.xss-lock}/bin/xss-lock -- "slock" &
      ${pkgs.xautolock}/bin/xautolock -detectsleep -locker "slock" &
      ${pkgs.feh}/bin/feh --bg-fill "$HOME/.wallpaper"
      ${pkgs.networkmanagerapplet}/bin/nm-applet &
      ${pkgs.volumeicon}/bin/volumeicon &
      ${pkgs.parcellite}/bin/parcellite &
      ${pkgs.cbatticon}/bin/cbatticon &
      ${pkgs.blueman}/bin/blueman-applet &
      ${pkgs.libinput-gestures}/bin/libinput-gestures &
    '';
  };

  security.wrappers.slock.source = "${pkgs.slock.out}/bin/slock";

  environment.variables = {
    XCURSOR_SIZE = "64";
    XCURSOR_PATH = [
      "${config.system.path}/share/icons"
      "$HOME/.icons"
      "$HOME/.nix-profile/share/icons/"
    ];
    GTK_DATA_PREFIX = [
      "${config.system.path}"
    ];
  };
}
