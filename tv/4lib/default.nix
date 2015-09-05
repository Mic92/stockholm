{ lib, pkgs, ... }:

let
  krebs = import ../../krebs/4lib { inherit lib; };
in

with krebs;

krebs // rec {

  git = import ./git.nix {
    lib = krebs;
    inherit pkgs;
  };

  # "7.4.335" -> "74"
  majmin = with lib; x : concatStrings (take 2 (splitString "." x));

  shell-escape = krebs.shell.escape;
}
