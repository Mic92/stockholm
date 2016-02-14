{ config, lib, pkgs, ... }:

with config.krebs.lib;
{
  services.teamviewer.enable = true;
}
