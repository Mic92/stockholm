{ lib, ... }:

with builtins;
with lib;

let out = rec {

  eq = x: y: x == y;
  ne = x: y: x != y;

  mod = x: y: x - y * (x / y);

  addName = name: set:
    set // { inherit name; };

  addNames = mapAttrs addName;

  types = import ./types.nix { inherit lib; };

  dir.has-default-nix = path: pathExists (path + "/default.nix");

  dns = import ./dns.nix { inherit lib; };
  genid = import ./genid.nix { lib = lib // out; };
  git = import ./git.nix { lib = lib // out; };
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

  mapAttrValues = f: mapAttrs (_: f);
  setAttr = name: value: set: set // { ${name} = value; };

}; in out
