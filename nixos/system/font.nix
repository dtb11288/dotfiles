{ pkgs, ... }:

{
  # Select internationalisation properties.
  console.keyMap = "us";
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "SourceCodePro" ]; })
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    freefont_ttf
    corefonts
    dejavu_fonts
    liberation_ttf
    siji
  ];
}
