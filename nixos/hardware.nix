{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/hardware/network/broadcom-43xx.nix>
      <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/81818a6e-5a78-47a5-89b1-aea31b4c76d9";
      fsType = "f2fs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/E2B9-D944";
      fsType = "vfat";
    };

  fileSystems."/tmp" =
    { device = "tmpfs";
      fsType = "tmpfs";
    };

  swapDevices = [ ];

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
    };
  };

  services.tlp.enable = true;

  nix.maxJobs = lib.mkDefault 8;
}
