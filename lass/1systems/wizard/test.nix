{ config, lib, pkgs, ... }:
{
  imports = [
    ./default.nix
  ];
  virtualisation.emptyDiskImages = [
    8000
  ];
  virtualisation.memorySize = 1024;
}
