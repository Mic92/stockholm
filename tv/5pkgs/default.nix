with import ../../lib;
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
      (pushBack "override"
        (attrNames
          (filterAttrs isNixDirEntry (readDir ./.))))))
