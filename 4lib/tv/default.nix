{ lib, pkgs, ... }:

with builtins;

let
  inherit (lib) mapAttrs stringAsChars;
in

rec {
  git = import ./git.nix {
    lib = lib // {
      inherit addNames;
    };
    inherit pkgs;
  };

  addName = name: set:
    set // { inherit name; };

  addNames = mapAttrs addName;


  # "7.4.335" -> "74"
  majmin = with lib; x : concatStrings (take 2 (splitString "." x));


  concat = xs :
    if xs == []
      then ""
      else head xs + concat (tail xs)
    ;

  flip = f : x : y : f y x;

  # isSuffixOf :: String -> String -> Bool
  isSuffixOf =
    s : xs :
      let
        sn = stringLength s;
        xsn = stringLength xs;
      in
        xsn >= sn && substring (xsn - sn) sn xs == s ;

  removeSuffix =
    s : xs : substring 0 (stringLength xs - stringLength s) xs;

  # setMap :: (String -> a -> b) -> Set String a -> [b]
  #setMap = f: xs: map (k : f k (getAttr k xs)) (attrNames xs);

  # setToList :: Set k a -> [a]
  #setToList = setMap (_: v: v);

  shell-escape =
    let
      isSafeChar = c: match "[-./0-9_a-zA-Z]" c != null;
    in
    stringAsChars (c:
      if isSafeChar c then c
      else if c == "\n" then "'\n'"
      else "\\${c}");

}
