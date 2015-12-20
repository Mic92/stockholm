{ config, lib, pkgs, ... }:

with lib;
{
  services.teamviewer.enable = true;
}
