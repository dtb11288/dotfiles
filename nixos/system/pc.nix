{ pkgs, lib, config, ... }:
{
  imports = [
    ./configuration.nix
    ./hardware-pc.nix
  ];

  hardware.logitech.wireless = {
    enable = true;
    enableGraphical = true;
  };

  hardware.bluetooth.enable = true;

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      libvdpau-va-gl
      vaapiVdpau
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
      libvdpau-va-gl
      vaapiVdpau
    ];
  };

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "nvidia-x11"
    ];

  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    modesetting.enable = true;
    open = true;
    nvidiaSettings = true;
    forceFullCompositionPipeline = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  environment.systemPackages = with pkgs; [
    ddcutil
  ];

  services.xserver.dpi = 144;
}
