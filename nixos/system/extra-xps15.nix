{ pkgs, ... }:

let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimu=NVIDIA_only
    exec -a "$0" "$@"
  '';
in
{
  hardware.logitech.wireless = {
    enable = true;
    enableGraphical = true;
  };

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

  services.xserver.videoDrivers = [ "modesetting" "nvidia" ];

  hardware.nvidia.prime = {
    offload.enable = true;

    # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
    intelBusId = "PCI:0:2:0";

    # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
    nvidiaBusId = "PCI:1:0:0";
  };

  environment.systemPackages = with pkgs; [
    nvidia-offload
    cbatticon
    libinput-gestures
  ];

  services.xserver.dpi = 192;

  programs.light.enable = true;

  services.xserver.displayManager.sessionCommands = ''${pkgs.cbatticon}/bin/cbatticon &'';
}
