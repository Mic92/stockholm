{ lib, ... }:

with lib;

rec {
  # tree k v = set k (either v (tree k v))

  # get : [k] -> tree k v -> v
  get = path: tree:
    if length path > 0
      then get (tail path) tree.${head path} # TODO check if elem exists
      else tree;
}
