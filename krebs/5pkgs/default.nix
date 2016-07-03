{ config, lib, pkgs, ... }@args:
with config.krebs.lib;
{
  nixpkgs.config.packageOverrides = pkgs: let

    # This callPackage will try to detect obsolete overrides.
    callPackage = path: args: let
      override = pkgs.callPackage path args;
      upstream = optionalAttrs (override ? "name")
        (pkgs.${(parseDrvName override.name).name} or {});
    in if upstream ? "name" &&
          override ? "name" &&
          compareVersions upstream.name override.name != -1
      then trace "Upstream `${upstream.name}' gets overridden by `${override.name}'." override
      else override;

  in {}
  // import ./builders.nix args
  // mapAttrs (_: flip callPackage {})
              (filterAttrs (_: dir.has-default-nix)
                           (subdirsOf ./.))
  // {
    empty = pkgs.runCommand "empty-1.0.0" {} "mkdir $out";

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

    ReaktorPlugins = callPackage ./Reaktor/plugins.nix {};

    buildbot = callPackage <nixpkgs/pkgs/development/tools/build-managers/buildbot> {
      inherit (pkgs.pythonPackages) twisted jinja2;
      dateutil = pkgs.pythonPackages.dateutil_1_5;
      sqlalchemy_migrate_0_7 = pkgs.pythonPackages.sqlalchemy_migrate_func (pkgs.pythonPackages.sqlalchemy7.override {
        doCheck = false;
      });
    };

    # XXX symlinkJoin changed arguments somewhere around nixpkgs d541e0d
    symlinkJoin = { name, paths, ... }@args: let
      x = pkgs.symlinkJoin args;
    in if typeOf x != "lambda" then x else pkgs.symlinkJoin name paths;

    test = {
      infest-cac-centos7 = callPackage ./test/infest-cac-centos7 {};
    };
  };
}
