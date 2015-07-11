let
  pkgs = import <nixpkgs> {};
  inherit (pkgs.lib) concatMap hasAttr;
in rec {

  no-touch-args = {
    config = throw "no-touch-args: can't touch config!";
    lib = throw "no-touch-args: can't touch lib!";
    pkgs = throw "no-touch-args: can't touch pkgs!";
  };

  # list-imports : path -> [path]
  # Return a module's transitive list of imports.
  # XXX duplicates won't get eliminated from the result.
  list-imports = path:
    let module = import path no-touch-args;
        imports = if hasAttr "imports" module
                    then concatMap list-imports module.imports
                    else [];
    in [path] ++ imports;
}
