with import <stockholm/lib>;

self: super:

# Import files and subdirectories like they are overlays.
foldl' mergeAttrs {}
  (map
    (name: import (./. + "/${name}") self super)
    (filter
      (name: name != "default.nix" && !hasPrefix "." name)
      (attrNames (readDir ./.))))

//

{
  ReaktorPlugins = self.callPackage ./simple/Reaktor/plugins.nix {};

  # https://github.com/proot-me/PRoot/issues/106
  proot = self.writeDashBin "proot" ''
    export PROOT_NO_SECCOMP=1
    exec ${super.proot}/bin/proot "$@"
  '';

  # XXX symlinkJoin changed arguments somewhere around nixpkgs d541e0d
  symlinkJoin = { name, paths, ... }@args: let
    x = super.symlinkJoin args;
  in if typeOf x != "lambda" then x else super.symlinkJoin name paths;
}
