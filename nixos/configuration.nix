{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware.nix
      ./user.nix
    ];

  # Select internationalisation properties.
  i18n = {
    # consoleFont = "Lat2-Terminus16";
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
    haskellPackages.taffybar
    xdg_utils
    i3lock
    cbatticon
    volumeicon
    parcellite
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

  services.xserver = {
    enable = true;
    layout = "us";
    videoDrivers = [ "intel" ];
    windowManager = {
      default = "xmonad";
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
          haskellPackages.taffybar
        ];
      };
    };

    libinput = {
      enable = true;
      naturalScrolling = true;
      disableWhileTyping = true;
    };

    displayManager.sessionCommands = ''
      ${pkgs.xorg.xrandr}/bin/xrandr --output eDP1 --mode 1920x1080
      ${pkgs.xorg.xset}/bin/xset r rate 220 30
      ${pkgs.xorg.xset}/bin/xset dpms 300
      ${pkgs.xorg.xrdb}/bin/xrdb -merge "$HOME/.Xresources"
      ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
      ${pkgs.networkmanagerapplet}/bin/nm-applet &
      ${pkgs.volumeicon}/bin/volumeicon &
      ${pkgs.cbatticon}/bin/cbatticon &
      ${pkgs.blueman}/bin/blueman-applet &
    '';
  };

  environment.variables = {
    VISUAL = "nvim";
  };

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    freefont_ttf
    dejavu_fonts
  ];

  programs.zsh.enable = true;
  programs.ssh.startAgent = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

}
