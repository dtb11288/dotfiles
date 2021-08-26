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
  boot.supportedFilesystems = [ "zfs" ];

  # Set your time zone.
  time.timeZone = "Asia/Ho_Chi_Minh";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp2s0.useDHCP = true;
  networking.hostId = "4eb6d6d2";

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
    git
  ];

  hardware = {
    logitech.wireless.enable = true;
    logitech.wireless.enableGraphical = true;
    cpu.intel.updateMicrocode = true;
    opengl.enable = true;
    opengl.driSupport32Bit = true;
    opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau ];
    opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiIntel libvdpau-va-gl vaapiVdpau ];
    pulseaudio.package = pkgs.pulseaudioFull; # 'full' instead of 'light' for e.g. bluetooth
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
    pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
    pulseaudio.daemon.config = {
      flat-volumes = "no";
    };
    bluetooth.enable = true;
  };

  nixpkgs.config.pulseaudio = true;

  nixpkgs.config.allowUnfree = true;
  services.openssh.enable = true;
  services.printing.enable = true;
  services.ntp.enable = true;
  virtualisation.docker.enable = true;
  #services.flatpak.enable = true;

  networking = {
    hostName = "nixos";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    extraHosts = ''
      127.0.0.1  biits.lambda
    '';
    networkmanager = {
      enable = true;
      insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
      packages = [ pkgs.networkmanager_openvpn ];
    };
  };

  services.blueman.enable = true;
  services.tlp.enable = true;

  environment.variables = {
    VISUAL = "nvim";
    PATH = "$PATH:$HOME/opt/bin";
  };

  programs.zsh.enable = true;
  programs.ssh.startAgent = true;

  nix.maxJobs = lib.mkDefault 8;

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
  system.stateVersion = "21.05"; # Did you read the comment?
}
