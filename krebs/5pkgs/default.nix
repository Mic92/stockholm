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
  brockman = self.haskellPackages.brockman;
  reaktor2 = self.haskellPackages.reaktor2;

  ReaktorPlugins = self.callPackage ./simple/Reaktor/plugins.nix {};
}
