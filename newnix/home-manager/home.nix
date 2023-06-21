# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)

{ inputs, lib, config, pkgs, ... }: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = (_: true);
    };
  };

  home = {
    username = "binh";
    homeDirectory = "/home/binh";
  };

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;
  home.packages = with pkgs; [
    gcc
    rustup
    silver-searcher
    ripgrep
    firefox
    google-chrome
    thunderbird
    lm_sensors
    pciutils
    slack
    dbeaver
    lshw
    gimp
    spotify-tui
    openssl
    libreoffice
    flameshot
    unzip
    unrar
    nomacs
    darktable
    (xfce.thunar.override { thunarPlugins = with pkgs; [ xfce.thunar-volman xfce.thunar-archive-plugin ]; })
    postman
    zathura
    mpv
    gimp
    kodi
    telegram-desktop
    lazydocker
  ];

  services.spotifyd = {
    enable = true;
    settings = {
      global = {
        username_cmd = "spotify_username";
        password_cmd = "spotify_password";
        device_name = "spotifyd";
        device_type = "computer";
        bitrate = 320;
        no_audio_cache = true;
        initial_volume = "100";
        backend = "pulseaudio";
      };
    };
  };

  # Enable home-manager and git
  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
