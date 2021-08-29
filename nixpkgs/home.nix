{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "binh";
  home.homeDirectory = "/home/binh";

  home.packages = with pkgs; [
    enpass
    firefox
    google-chrome
    rustup
    dbeaver
    tdesktop
    slack
    silver-searcher
    deadbeef
    thunderbird
    lm_sensors
    pciutils
    xfce.thunar
    lshw
    nmap-graphical
    wireshark
    transmission-gtk
    zoom
    zathura
    mpv
    rclone
    postman
    gimp
    spotify
    virt-manager
    light
    mariadb-client
    nodejs-14_x
    rust-analyzer
    pkg-config
    openssl
    haskell-language-server
    libreoffice
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";
}
