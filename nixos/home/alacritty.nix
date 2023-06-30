{ theme, ... }:
{
  programs.alacritty = {
    enable = true;
    settings = {
      env.TERM = "alacritty";
      font = {
        normal = {
          familly = "SauceCodePro Nerd Font";
          style = "Regular";
        };
        size = 10.0;
      };
      colors = with theme.colors; {
        primary = {
          background = background;
          foreground = foreground;
        };

        cursor = {
          text = background;
          cursor = foreground;
        };

        normal = {
          black = color0;
          red = color1;
          green = color2;
          yellow = color3;
          blue = color4;
          magenta = color5;
          cyan = color6;
          white = color7;
        };

        bright = {
          black = color8;
          red = color9;
          green = color10;
          yellow = color11;
          blue = color12;
          magenta = color13;
          cyan = color14;
          white = color15;
        };
      };
    };
  };
}
