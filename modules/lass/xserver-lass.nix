{ config, pkgs, ... }:

{
  imports = [
    ../tv/xserver.nix
  ];
  services.xserver.displayManager.auto.user = "lass";
}
