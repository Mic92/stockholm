{ lib, pkgs, test, ... }:
if test then {
  nixpkgs-unstable = lib.mkForce { file = "/var/empty"; };
} else {}
