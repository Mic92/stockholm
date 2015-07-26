{ lib, ... }:

let
  listset = import ./listset.nix { inherit lib; };
in

with builtins;
with lib;

rec {
  # label = string

  # TODO does it make sense to have alias = list label?

  # split-by-provider :
  #   [[label]] -> tree label provider -> listset provider alias
  split-by-provider = as: providers:
    foldl (m: a: listset.insert (provider-of a providers) a m) {} as;

  # provider-of : alias -> tree label provider -> provider
  # Note that we cannot use tree.get here, because path can be longer
  # than the tree depth.
  provider-of = a:
    let
      go = path: tree:
        if typeOf tree == "string"
          then tree
          else go (tail path) tree.${head path};
    in
    go (reverseList (splitString "." a));
}
