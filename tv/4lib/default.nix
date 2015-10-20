{ lib, pkgs, ... }:

lib // rec {

  git = import ./git.nix {
    inherit lib pkgs;
  };

  # "7.4.335" -> "74"
  majmin = with lib; x : concatStrings (take 2 (splitString "." x));

  # TODO deprecate shell-escape for lass
  shell-escape = lib.shell.escape;
}
