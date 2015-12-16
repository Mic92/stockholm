{ config, lib, pkgs, ... }:

with lib;
{
  environment.systemPackages = with pkgs;[
    get
    cac
    cacpanel
    jq
  ];
}
