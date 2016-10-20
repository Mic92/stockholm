let
  nixpkgs-lib = import <nixpkgs/lib>;
  lib = with lib; nixpkgs-lib // builtins // {
    git = import ./git.nix { inherit lib; };
    shell = import ./shell.nix { inherit lib; };
    types = nixpkgs-lib.types // import ./types.nix { inherit lib; };

    eq = x: y: x == y;
    ne = x: y: x != y;
    mod = x: y: x - y * (x / y);

    genid = import ./genid.nix { inherit lib; };
    genid_signed = x: ((lib.genid x) + 16777216) / 2;

    lpad = n: c: s:
      if lib.stringLength s < n
        then lib.lpad n c (c + s)
        else s;

    subdirsOf = path:
      lib.mapAttrs (name: _: path + "/${name}")
                   (filterAttrs (_: eq "directory") (readDir path));

    genAttrs' = names: f: listToAttrs (map f names);

    getAttrs = names: set:
      listToAttrs (map (name: nameValuePair name set.${name})
                       (filter (flip hasAttr set) names));

    setAttr = name: value: set: set // { ${name} = value; };

    toC = x: let
      type = typeOf x;
      reject = throw "cannot convert ${type}";
    in {
      list = "{ ${concatStringsSep ", " (map toC x)} }";
      null = "NULL";
      set = if isDerivation x then toJSON x else reject;
      string = toJSON x; # close enough
    }.${type} or reject;

  };
in

lib
