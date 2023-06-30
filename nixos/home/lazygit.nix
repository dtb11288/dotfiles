{ ... }:
{
  programs.lazygit = {
    enable = true;
    settings = {
      gui = {
        showIcons = true;
        showFileTree = false;
      };
    };
  };
}
