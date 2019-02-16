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
  reaktor2 = self.haskellPackages.reaktor2;

  ReaktorPlugins = self.callPackage ./simple/Reaktor/plugins.nix {};

  # https://github.com/proot-me/PRoot/issues/106
  proot = self.writeDashBin "proot" ''
    export PROOT_NO_SECCOMP=1
    exec ${super.proot}/bin/proot "$@"
  '';
}
