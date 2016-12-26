{ config, lib, pkgs, ... }:

{
  users.users.mainUser.extraGroups = [ "libvirtd" ];
  virtualisation.libvirtd.enable = true;

}
