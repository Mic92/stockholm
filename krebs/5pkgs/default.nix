pkgs: oldpkgs:
with import <stockholm/lib>;

  foldl' mergeAttrs {}
    (map
      (name: import (./. + "/${name}") pkgs oldpkgs)
      (filter
        (name: name != "default.nix" && !hasPrefix "." name)
        (attrNames (readDir ./.))))

  // {
    ReaktorPlugins = pkgs.callPackage ./simple/Reaktor/plugins.nix {};

    buildbot-full = pkgs.callPackage ./simple/buildbot {
      plugins = with pkgs.buildbot-plugins; [ www console-view waterfall-view ];
    };
    buildbot-worker = pkgs.callPackage ./simple/buildbot/worker.nix {};

    # https://github.com/proot-me/PRoot/issues/106
    proot = pkgs.writeDashBin "proot" ''
      export PROOT_NO_SECCOMP=1
      exec ${oldpkgs.proot}/bin/proot "$@"
    '';

    # XXX symlinkJoin changed arguments somewhere around nixpkgs d541e0d
    symlinkJoin = { name, paths, ... }@args: let
      x = oldpkgs.symlinkJoin args;
    in if typeOf x != "lambda" then x else oldpkgs.symlinkJoin name paths;
  }
