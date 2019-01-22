# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware.nix
      ./user.nix
    ];

  # Select internationalisation properties.
  i18n = {
    consoleFont = "sun12x22";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Asia/Ho_Chi_Minh";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # tools
    git
    neovim
    file
    coreutils
    tmux
    htop
    wget
    curl
    tree
    w3m
    cmus
    ranger
    ag
    zip
    unzip
    lsof
    xdg_utils
    rsync
    feh
    binutils
    gcc
    gnumake
    openssl
    pkgconfig

    # desktop
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
    wmctrl
    enpass
  ];

  nixpkgs.config.allowUnfree = true;

  services.redshift = {
    enable = true;
    latitude = "21";
    longitude = "105";
    temperature = {
      day = 4100;
      night = 3100;
    };
  };

  services.openssh.enable = true;
  services.printing.enable = true;
  services.ntp.enable = true;
  virtualisation.docker.enable = true;

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
    '';
  };

  security.wrappers.slock.source = "${pkgs.slock.out}/bin/slock";

  environment.variables = {
    VISUAL = "nvim";
    PATH = "$PATH:$HOME/opt/bin";
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

  fonts.fontconfig.dpi = 180;
  fonts.fonts = with pkgs; [
    source-code-pro
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    freefont_ttf
    dejavu_fonts
  ];

  programs.zsh.enable = true;
  programs.ssh.startAgent = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}
