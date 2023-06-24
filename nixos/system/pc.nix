{ ... }: {
  imports = [
    ./configuration.nix

    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-pc.nix
  ];
}
