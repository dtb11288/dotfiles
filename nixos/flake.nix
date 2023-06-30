{
  description = "Your new nix config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";

    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    theme = {
      url = "path:theme.nix";
      flake = false;
    };
  };

  outputs = { nixpkgs, theme, home-manager, ... }@inputs: {
    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .#your-hostname'
    nixosConfigurations = {
      xps15 = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs;
          hostname = "xps15";
          username = "binh";
        }; # Pass flake inputs to our config
        # > Our main nixos configuration file <
        modules = [ ./system/xps15.nix ];
      };
      pc = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs;
          hostname = "pc";
          username = "binh";
        }; # Pass flake inputs to our config
        # > Our main nixos configuration file <
        modules = [ ./system/pc.nix ];
      };
    };

    # Standalone home-manager configuration entrypoint
    # Available through 'home-manager --flake .#your-username@your-hostname'
    homeConfigurations = {
      "binh@xps15" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires 'pkgs' instance
        extraSpecialArgs = {
          inherit inputs;
          theme = (import theme);
          hostname = "xps15";
          username = "binh";
        }; # Pass flake inputs to our config
        # > Our main home-manager configuration file <
        modules = [ ./home/home.nix ];
      };
      "binh@pc" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires 'pkgs' instance
        extraSpecialArgs = {
          inherit inputs;
          theme = (import theme);
          hostname = "pc";
          username = "binh";
        }; # Pass flake inputs to our config
        # > Our main home-manager configuration file <
        modules = [ ./home/home.nix ];
      };
    };
  };
}
