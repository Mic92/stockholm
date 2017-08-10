with import <stockholm/lib>;
let
  overrides = self: super:
    listToAttrs
      (map
        (name: nameValuePair (removeSuffix ".nix" name)
                             (self.callPackage (./. + "/${name}") {}))
        (filter
          (name: name != "default.nix" && !hasPrefix "." name)
          (attrNames (readDir ./.))));
in
self: super:
{
  haskell.packages = mapAttrs (_: pkgs: pkgs.override {
    inherit overrides;
  }) super.haskell.packages;
  haskellPackages = super.haskellPackages.override {
    inherit overrides;
  };
}
