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
  haskell = super.haskell // {
    packages = mapAttrs (name: value:
      if hasAttr "override" value
        then value.override { inherit overrides; }
        else value
    ) super.haskell.packages;
  };
  haskellPackages = super.haskellPackages.override {
    inherit overrides;
  };
}
