{ config, lib, pkgs, ... }:
with import <stockholm/lib>;
{
  imports = [
    ./3modules
  ];
  nixpkgs.config.packageOverrides = import ./5pkgs pkgs;
}
