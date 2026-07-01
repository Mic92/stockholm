{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-writers.url = "git+https://cgit.krebsco.de/nix-writers";
    nix-writers.inputs.nixpkgs.follows = "nixpkgs";
    # disko.url = "github:nix-community/disko";
    # disko.inputs.nixpkgs.follows = "nixpkgs";
    buildbot-nix.url = "github:Mic92/buildbot-nix";
    buildbot-nix.inputs.nixpkgs.follows = "nixpkgs";
    kartei.url = "github:Mic92/kartei";
    kartei.flake = false;
  };

  description = "stockholm";

  outputs = { self, nixpkgs, nix-writers, buildbot-nix, kartei, ... }: {
    nixosConfigurations = nixpkgs.lib.mapAttrs (machineName: _: nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs.stockholm = self;
      specialArgs.nix-writers = nix-writers;
      specialArgs.buildbot-nix = buildbot-nix;
      specialArgs.kartei = kartei;
      modules = [
        ./krebs/1systems/${machineName}/config.nix
        {
          krebs.secret.directory = "/var/src/secrets";
        }
      ];
    }) (builtins.readDir ./krebs/1systems);

    nixosModules =
    let
      inherit (nixpkgs) lib;
    in builtins.listToAttrs
      (map
        (name: {name = lib.removeSuffix ".nix" name; value = import (./krebs/3modules + "/${name}");})
        (lib.filter
          (name: name != "default.nix" && !lib.hasPrefix "." name)
          (lib.attrNames (builtins.readDir ./krebs/3modules))));

    kartei = {
      hosts = self.nixosConfigurations.hotdog.config.krebs.hosts;
      users = self.nixosConfigurations.hotdog.config.krebs.users;
    };
    overlays.default = import ./krebs/5pkgs/default.nix;
    packages = let
      allNames = self.lib.attrNames (self.lib.mapNixDir (x: null) ./krebs/5pkgs/simple);
      appliedOverlay = (system:
        let
          base = self.inputs.nixpkgs.legacyPackages.${system};
          # Apply nix-writers overlay with fixpoint so its functions can find each other
          withWriters = nixpkgs.lib.fix (final: base // nix-writers.overlays.default final base);
        in self.overlays.default {} (withWriters // { lib = self.lib; }));
      # Only include derivations in packages output
      getDerivations = overlay: builtins.listToAttrs (builtins.filter (x: x != null) (map (name:
        let val = overlay.${name} or null;
        in if val != null && (val.type or null) == "derivation"
           then { inherit name; value = val; }
           else null
      ) allNames));
    in nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" ] (system: getDerivations (appliedOverlay system));
    lib = import (self.outPath + "/lib/pure.nix") { lib = nixpkgs.lib; };
  };
}
