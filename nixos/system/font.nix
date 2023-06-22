{ pkgs, ... }:

{
  # Select internationalisation properties.
  console.font = "sun12x22";
  console.keyMap = "us";
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "SourceCodePro" ]; })
    #source-code-pro
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    freefont_ttf
    corefonts
    dejavu_fonts
    liberation_ttf
  ];
}
