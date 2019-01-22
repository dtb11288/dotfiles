{ pkgs, ... }:

{
  # Select internationalisation properties.
  i18n = {
    consoleFont = "sun12x22";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  fonts = {
    fontconfig.dpi = 192;
    fonts = with pkgs; [
      source-code-pro
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      freefont_ttf
      dejavu_fonts
      liberation_ttf
    ];
  };
}
