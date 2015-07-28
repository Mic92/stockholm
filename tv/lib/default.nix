{ lib, pkgs, ... }:

let
  krebs = import ../../4lib/krebs { inherit lib; };
in

with krebs;

krebs // rec {

  git = import ./git.nix {
    lib = krebs;
    inherit pkgs;
  };

  # "7.4.335" -> "74"
  majmin = with lib; x : concatStrings (take 2 (splitString "." x));

  shell-escape =
    let
      isSafeChar = c: match "[-./0-9_a-zA-Z]" c != null;
    in
    stringAsChars (c:
      if isSafeChar c then c
      else if c == "\n" then "'\n'"
      else "\\${c}");
}
