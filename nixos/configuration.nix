# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "nixos";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    networkmanager = {
      enable = true;
      insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
    };
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    bumblebee.enable = true;
    opengl.enable = true;
    opengl.driSupport32Bit = true;
    opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau ];
    opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiIntel libvdpau-va-gl vaapiVdpau ];
    pulseaudio.package = pkgs.pulseaudioFull; # 'full' instead of 'light' for e.g. bluetooth
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
    pulseaudio.daemon.config = {
      flat-volumes = "no";
    };
    bluetooth.enable = true;
  };

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
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

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
  services.tlp.enable = true;

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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.binh = {
    createHome = true;
    home = "/home/binh";
    group = "users";
    extraGroups = [ "wheel" "disk" "networkmanager" "video" "audio" "input" ];
    isNormalUser = true;
    uid = 1000;
    useDefaultShell = true;
    shell = pkgs.zsh;
  };

  security.sudo.enable = true;
  programs.zsh.enable = true;
  programs.ssh.startAgent = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

}
