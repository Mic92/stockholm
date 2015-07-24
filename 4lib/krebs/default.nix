{ lib, ... }:

with builtins;
with lib;

builtins // lib // rec {

  addName = name: set:
    set // { inherit name; };

  addNames = mapAttrs addName;

  types = import ./types.nix { inherit lib; };

}
