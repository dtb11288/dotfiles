{ pkgs, ... }:
{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
    plugins = with pkgs; [ rofi-calc rofi-emoji ];
    extraConfig = {
      modi = "drun,run,combi";
      show-icons = true;
    };
    theme = "paper-float";
  };
}
