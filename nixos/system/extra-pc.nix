{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    ddcutil
  ];

  services.xserver.dpi = 144;
}
