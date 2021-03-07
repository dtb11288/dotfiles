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
    playerctl
    feh

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

  services.xserver = {
    dpi = 192;
    enable = true;
    layout = "us";
    videoDrivers = [ "intel" ];

    libinput = {
      enable = true;
      naturalScrolling = true;
      disableWhileTyping = true;
    };

    windowManager = {
      qtile.enable = true;
    };

    desktopManager = {
      xterm.enable = false;
    };

    displayManager = {
      defaultSession = "none+qtile";
      lightdm = {
        enable = true;
      };
      autoLogin.enable = true;
      autoLogin.user = "binh";

      sessionCommands = ''
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
  };

  security.wrappers.slock.source = "${pkgs.slock.out}/bin/slock";

  environment.variables = {
    #XCURSOR_SIZE = "64";
    #XCURSOR_PATH = [
    #  "${config.system.path}/share/icons"
    #  "$HOME/.icons"
    #  "$HOME/.nix-profile/share/icons/"
    #];
    #GTK_DATA_PREFIX = [
    #  "${config.system.path}"
    #];
  };
}
