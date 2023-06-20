# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ./user.nix
      ./font.nix
      ./xsession.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

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
    zellij
    htop
    wget
    curl
    git
    alsaUtils
    fzf
    lazygit
    ripgrep
  ];

  hardware.logitech.wireless = {
    enable = true;
    enableGraphical = true;
  };

  hardware.cpu.intel.updateMicrocode = true;
  hardware.bluetooth.enable = true;

  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      libvdpau-va-gl
      vaapiVdpau
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
      vaapiIntel
      libvdpau-va-gl
      vaapiVdpau
    ];
  };

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  nixpkgs.config.allowUnfree = true;
  services.openssh.enable = true;
  services.printing.enable = true;
  services.ntp.enable = true;
  virtualisation.docker = {
    enable = true;
  };
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  services.flatpak.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal
      xdg-desktop-portal-gtk
    ];
  };

  networking = {
    hostName = "nixos";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    extraHosts = ''
      127.0.0.1  biits.lambda
    '';
    networkmanager = {
      enable = true;
      insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
      plugins = with pkgs; [ networkmanager-openvpn ];
    };
  };

  services.blueman.enable = true;
  services.tlp.enable = true;

  services.gvfs.enable = true;
  services.tumbler.enable = true;

  environment.variables = {
    VISUAL = "nvim";
    PATH = "$PATH:$HOME/opt/bin:$HOME/.cargo/bin:/var/lib/flatpak/exports/bin";
  };

  programs.zsh.enable = true;
  programs.ssh.startAgent = true;

  nix.settings.max-jobs = lib.mkDefault 8;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
