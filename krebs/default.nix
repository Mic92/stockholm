{ config, lib, pkgs, ... }:
with config.krebs.lib;
{
  imports = [
    ./3modules
    ./5pkgs
  ];
}
