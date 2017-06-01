with import <stockholm/lib>;

self: super:
{
  haskellPackages = super.haskellPackages.override {
    overrides = self: super:
      listToAttrs
        (map
          (name: nameValuePair (removeSuffix ".nix" name)
                               (self.callPackage (./. + "/${name}") {}))
          (filter
            (name: name != "default.nix" && !hasPrefix "." name)
            (attrNames (readDir ./.))));
  };
}
