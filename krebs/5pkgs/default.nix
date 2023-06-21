self: super: let
  stockholm = {
    lib = import ../../lib/pure.nix { lib = super.lib; };
    outPath = toString ../.;
  };
in
with stockholm.lib;

fix (foldl' (flip extends) (self: super) (
  [
    (self: super: { inherit stockholm; })
  ]
  ++
  (map
    (name: import (./. + "/${name}"))
    (filter
      (name: name != "default.nix" && !hasPrefix "." name)
      (attrNames (readDir ./.))))
  ++
  [
    (self: super: {
      brockman = self.haskellPackages.brockman;
      reaktor2 = self.haskellPackages.reaktor2;
    })
  ]
))
