with import <stockholm/lib>;
let
  pushBack = x: xs:
    if elem x xs then
      remove x xs ++ [ x ]
    else
      xs;
in

self: super:

# Import files and subdirectories like they are overlays.
fix
  (foldl' (flip extends) (_: super)
    (map
      (name: import (./. + "/${name}"))
      (filter
        (name: name != "default.nix" && !hasPrefix "." name)
        (pushBack "override"
          (attrNames (readDir ./.))))))
