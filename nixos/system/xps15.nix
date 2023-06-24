{ ... }: {
  imports = [
    ./configuration.nix

    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-xps15.nix
    ./nvidia-xps15.nix
  ];
}
