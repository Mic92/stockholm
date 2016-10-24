{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
{
  services.teamviewer.enable = true;
}
