{ lib, pkgs, test, ... }:
if test then {} else {
  nixpkgs-unstable = lib.mkForce { file = "/var/empty"; };
}
