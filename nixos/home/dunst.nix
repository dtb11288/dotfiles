{ theme, ... }:
{
  services.dunst = {
    enable = true;
    settings = with theme.colors; {
      global = {
        format = "<b>%s %p</b>\n%b\n%p";
        geometry = "0x0-20+50";
        transparency = 1;
        separator_height = 2;
        padding = 16;
        horizontal_padding = 32;
        line_height = 1;
        frame_width = 2;
        alignment = "center";
        frame_color = color4;
        font = "Noto Font 14";
        word_wrap = "yes";
        max_icon_size = 64;
      };
      urgency_low = {
        msg_urgency = "low";
        background = background;
        foreground = foreground;
        timeout= 4;
      };
      urgency_normal = {
        msg_urgency = "normal";
        background = background;
        foreground = color15;
        timeout = 4;
      };
      urgency_critical = {
        msg_urgency = "critical";
        background = color0;
        foreground = color9;
        timeout = 0;
      };
    };
  };
}
