{ lib, pkgs, ... }:

with lib;

lib // rec {

  git = import ./git.nix {
    inherit lib pkgs;
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
