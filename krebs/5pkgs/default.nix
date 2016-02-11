{ lib, pkgs, ... }@args:
with lib;
{
  nixpkgs.config.packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super:
        mapAttrs (name: path: self.callPackage path {})
          (mapAttrs'
            (name: type:
              if hasSuffix ".nix" name
                then {
                  name = removeSuffix ".nix" name;
                  value = ./haskell-overrides + "/${name}";
                }
                else null)
            (builtins.readDir ./haskell-overrides));
    };

    push = pkgs.callPackage ./push {
      inherit (subdirs) get jq;
    };

    ReaktorPlugins = pkgs.callPackage ./Reaktor/plugins.nix {};

    test = {
      infest-cac-centos7 = pkgs.callPackage ./test/infest-cac-centos7 {};
    };
  }
  // import ./builders.nix args
  // mapAttrs (_: flip pkgs.callPackage {})
              (filterAttrs (_: dir.has-default-nix)
                           (subdirsOf ./.));
}
