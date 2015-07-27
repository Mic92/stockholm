{ lib, ... }:

with builtins;
with lib;

builtins // lib // rec {

  addName = name: set:
    set // { inherit name; };

  addNames = mapAttrs addName;

  types = import ./types.nix { inherit lib; };


  # listset k v = set k [v]

  # listset-insert : k -> v -> listset k v -> listset k v
  listset-insert = name: value: set:
    set // { ${name} = set.${name} or [] ++ [value]; };

  # tree k v = set k (either v (tree k v))

  # tree-get : [k] -> tree k v -> v
  tree-get = path: x:
    let
      y = x.${last path};
    in
    if typeOf y != "set"
      then y
      else tree-get (init path) y;

}
