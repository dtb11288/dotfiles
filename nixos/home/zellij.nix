{ theme, ... }:
{
  programs.zellij = {
    enable = true;
    settings = {
      themes = {
        default = with theme.colors; {
          fg = foreground;
          bg = background;
          black = color0;
          red = color1;
          green = color2;
          yellow = color3;
          blue = color4;
          magenta = color5;
          cyan = color6;
          white = color7;
          orange = color1;
        };
      };
      theme = "default";
      default_mode = "locked";
      default_layout = "compact"; # default or compact
      simplified_ui = true;
      pane_frames = false;
      plugins = {
        tab-bar.path = "tab-bar";
        status-bar.path = "status-bar";
        strider.path = "strider";
        compact-bar.path = "compact-bar";
      };
    };
  };
}
