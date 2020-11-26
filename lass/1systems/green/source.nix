{ lib, pkgs, ... }:
{
  nixpkgs-unstable = lib.mkForce { file = "/var/empty"; };
  nixpkgs.git.shallow = true;
}
