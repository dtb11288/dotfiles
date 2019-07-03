# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware.nix
      ./user.nix
      ./font.nix
      ./xsession.nix
    ];

  # Set your time zone.
  time.timeZone = "Asia/Ho_Chi_Minh";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    bind
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
    binutils
    gcc
    gnumake
    openssl
    pkgconfig
  ];

  nixpkgs.config.allowUnfree = true;
  services.openssh.enable = true;
  services.printing.enable = true;
  services.ntp.enable = true;
  virtualisation.docker.enable = true;
  services.flatpak.enable = true;

  environment.variables = {
    VISUAL = "nvim";
    PATH = "$PATH:$HOME/opt/bin";
  };

  programs.zsh.enable = true;
  programs.ssh.startAgent = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
