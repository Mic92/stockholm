{ lib, ... }:

with builtins;
with lib;

rec {

  eq = x: y: x == y;

  addName = name: set:
    set // { inherit name; };

  addNames = mapAttrs addName;

  types = import ./types.nix { inherit lib; };

  dns = import ./dns.nix { inherit lib; };
  listset = import ./listset.nix { inherit lib; };
  shell = import ./shell.nix { inherit lib; };
  tree = import ./tree.nix { inherit lib; };

  toC = x: {
    list = "{ ${concatStringsSep ", " (map toC x)} }";
    null = "NULL";
    string = toJSON x; # close enough
  }.${typeOf x};

  subdirsOf = path:
    mapAttrs (name: _: path + "/${name}")
             (filterAttrs (_: eq "directory") (readDir path));
}
