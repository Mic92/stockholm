{ lib, pkgs, ... }:
{
  nixpkgs.git.ref = lib.mkForce (lib.importJSON ../../../krebs/nixpkgs-unstable.json).rev;
}
