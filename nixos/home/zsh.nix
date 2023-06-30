{ pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    shellAliases = {
      vi = "${pkgs.neovim}/bin/nvim";
      vim = "${pkgs.neovim}/bin/nvim";
    };
    zplug = {
      enable = true;
      plugins = [
        { name = "zsh-users/zsh-autosuggestions"; }
        { name = "zsh-users/zsh-completions"; }
        { name = "spwhitt/nix-zsh-completions"; }
      ];
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "wd" "git" "npm" "cp" "docker" "docker-compose" "rust" "history" ];
      theme = "gentoo";
    };
    initExtra = ''
      prompt_nix_shell_setup
      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
    '';
  };
}
