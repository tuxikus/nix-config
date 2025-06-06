{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/nix-darwin-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      darwin,
      disko,
      ...
    }@inputs:
    {
      ### Hosts
      # zeus
      nixosConfigurations.zeus = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = inputs;
        modules = [
          disko.nixosModules.disko
          ./hosts/zeus/configuration.nix
          ./hosts/zeus/disks.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.tuxikus = import ./hosts/zeus/home.nix;
            home-manager.sharedModules = [ ];
          }
        ];
      };
      # aphrodite
      darwinConfigurations.aphrodite = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./hosts/aphrodite/configuration.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users."dominik.potoczki" = import ./hosts/aphrodite/home.nix;
            home-manager.sharedModules = [ ];
          }
        ];
      };
    };
}
