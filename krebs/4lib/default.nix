_:

let
  lib = import <stockholm/lib>;
in

with lib;

let out = lib // rec {

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
  genid_signed = x: ((genid x) + 16777216) / 2;
  git = import ./git.nix { lib = lib // out; };
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

  getAttrs = names: set:
    listToAttrs (map (name: nameValuePair name set.${name})
                     (filter (flip hasAttr set) names));

  setAttr = name: value: set: set // { ${name} = value; };

  optionalTrace = c: msg: x: if c then trace msg x else x;

}; in out
