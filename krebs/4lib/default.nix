{ lib, ... }:

with builtins;
with lib;

builtins // lib // rec {

  addName = name: set:
    set // { inherit name; };

  addNames = mapAttrs addName;

  types = import ./types.nix { inherit lib; };

  dns = import ./dns.nix { inherit lib; };
  listset = import ./listset.nix { inherit lib; };
  shell = import ./shell.nix { inherit lib; };
  tree = import ./tree.nix { inherit lib; };
}
