pkgs: oldpkgs:
with import <stockholm/lib>;
  let
    # This callPackage will try to detect obsolete overrides.
    callPackage = path: args: let
      override = pkgs.callPackage path args;
      upstream = optionalAttrs (override ? "name")
        (oldpkgs.${(parseDrvName override.name).name} or {});
    in if upstream ? "name" &&
          override ? "name" &&
          compareVersions upstream.name override.name != -1
      then trace "Upstream `${upstream.name}' gets overridden by `${override.name}'." override
      else override;
  in {}
  // import ./simple pkgs oldpkgs
  // import ./writers.nix pkgs oldpkgs
  // {
    haskellPackages = oldpkgs.haskellPackages.override {
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

    ReaktorPlugins = callPackage ./simple/Reaktor/plugins.nix {};

    buildbot-full = callPackage ./simple/buildbot {
      plugins = with pkgs.buildbot-plugins; [ www console-view waterfall-view ];
    };
    buildbot-worker = callPackage ./simple/buildbot/worker.nix {};

    # https://github.com/proot-me/PRoot/issues/106
    proot = pkgs.writeDashBin "proot" ''
      export PROOT_NO_SECCOMP=1
      exec ${oldpkgs.proot}/bin/proot "$@"
    '';

    # XXX symlinkJoin changed arguments somewhere around nixpkgs d541e0d
    symlinkJoin = { name, paths, ... }@args: let
      x = oldpkgs.symlinkJoin args;
    in if typeOf x != "lambda" then x else oldpkgs.symlinkJoin name paths;

    test = {
      infest-cac-centos7 = callPackage ./test/infest-cac-centos7 {};
    };
  }
