{ lib, pkgs, ... }:

lib // rec {

  git = import ./git.nix {
    inherit lib pkgs;
  };

  # TODO deprecate shell-escape for lass
  shell-escape = lib.shell.escape;
}
