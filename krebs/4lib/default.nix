{ config, lib, ... }:

with builtins;
with lib;

let out = rec {

  eq = x: y: x == y;
  ne = x: y: x != y;

  mod = x: y: x - y * (x / y);

  addName = name: set:
    set // { inherit name; };

  addNames = mapAttrs addName;

  guard = spec@{ type, value, ... }:
    assert isOptionType type;
    if type.check value
      then value
      else throw (toString (filter isString [
        "argument"
        (if spec ? name then "‘${spec.name}’" else null)
        "is not a ${type.name}"
      ]));

  types = import ./types.nix {
    inherit config;
    lib = lib // { inherit genid optionalTrace; };
  };

  dir.has-default-nix = path: pathExists (path + "/default.nix");

  genid = import ./genid.nix { lib = lib // out; };
  git = import ./git.nix { lib = lib // out; };
  shell = import ./shell.nix { inherit lib; };
  tree = import ./tree.nix { inherit lib; };

  lpad = n: c: s:
    if stringLength s < n
      then lpad n c (c + s)
      else s;

  toC = x: let
    type = typeOf x;
    reject = throw "cannot convert ${type}";
  in {
    list = "{ ${concatStringsSep ", " (map toC x)} }";
    null = "NULL";
    set = if isDerivation x then toJSON x else reject;
    string = toJSON x; # close enough
  }.${type} or reject;

  subdirsOf = path:
    mapAttrs (name: _: path + "/${name}")
             (filterAttrs (_: eq "directory") (readDir path));

  genAttrs' = names: f: listToAttrs (map f names);

  setAttr = name: value: set: set // { ${name} = value; };

  optionalTrace = c: msg: x: if c then trace msg x else x;

}; in out
