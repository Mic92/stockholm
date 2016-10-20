{ config, lib, pkgs, ... }:
with import <stockholm/lib>;
{
  imports = [
    ./3modules
    ./5pkgs
  ];
}
