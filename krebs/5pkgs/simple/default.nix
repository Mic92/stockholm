with import <stockholm/lib>;

self: super:
listToAttrs
  (map
    (name: nameValuePair (removeSuffix ".nix" name)
                         (super.callPackage (./. + "/${name}") {}))
    (filter
      (name: name != "default.nix" && !hasPrefix "." name)
      (attrNames (readDir ./.))))
