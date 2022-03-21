{ lib }:

with builtins;

rec {

  # Derive a file by substituting
  # "${pkgs.foo}/bin/foo" for each {-pkg-}"foo", and
  # "${pkgs.bar}/bin/foo" for each {-pkg:bar-}"foo".
  # If a package doesn't exist, a warning gets printed.
  substitutePkgs = name: { callsite ? null, pkgs, path }:
    let
      sourceDescription =
        if callsite != null then
          "${name} in ${toString callsite}"
        else
          "${name} from ${toString path}";

      f = dependencies: s:
        let
          parse = match "(.*)([{]-pkg(:([^}]+))?-[}]\"([^\"]+)\")(.*)" s;
          prefix = elemAt parse 0;
          pname = if elemAt parse 3 != null then elemAt parse 3 else exename;
          exename = elemAt parse 4;
          suffix = elemAt parse 5;
          pkg = pkgs.${pname} or null;

          substitute =
            if pkg != null then
              "${pkg}/bin/${exename}"
            else
              trace (toString [
                "lib.haskell.substitutePkgs:"
                "warning:"
                "while deriving ${sourceDescription}:"
                "no substitute found for ${elemAt parse 1}"
              ])
              exename;
        in
        if parse == null then
          (pkgs.writeText name s).overrideAttrs (old: {
            dependencies = old.dependencies or [] ++ dependencies;
          })

        else
          f (dependencies ++ [pkg]) (prefix + toJSON substitute + suffix);
    in
    f [] (readFile path);
}
