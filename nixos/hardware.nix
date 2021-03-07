{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/hardware/network/broadcom-43xx.nix>
      <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" "aes_x86_64" "aesni_intel" "cryptd" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.supportedFilesystems = [ "ntfs" ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/725f38ad-a9b1-4261-acb7-2f3a9c6e8e66";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."nixos".device = "/dev/disk/by-uuid/58044f04-198c-4ab9-9837-4e4e7369ccc1";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/EF46-339B";
      fsType = "vfat";
    };

  fileSystems."/tmp" =
    { device = "tmpfs";
      fsType = "tmpfs";
    };

  swapDevices = [ ];

  hardware = {
    logitech.wireless.enable = true;
    logitech.wireless.enableGraphical = true;
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

  nixpkgs.config.pulseaudio = true;

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

  services.tlp.enable = true;

  nix.maxJobs = lib.mkDefault 8;
}
