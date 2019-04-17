with import <stockholm/lib>;
{ config, lib, pkgs, ... }:
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/mail.nix>

    #<stockholm/lass/2configs/blue.nix>
    <stockholm/lass/2configs/syncthing.nix>
  ];

  krebs.build.host = config.krebs.hosts.green;

  krebs.syncthing.folders = [
    { id = "contacts"; path = "/home/lass/contacts"; peers = [ "mors" "blue" "green" "phone" ]; }
    { path = "/home/lass/.weechat"; peers = [ "blue" "green" "mors" ]; }
  ];
  lass.ensure-permissions = [
    { folder = "/home/lass/contacts"; owner = "lass"; group = "syncthing"; }
    { folder = "/home/lass/.weechat"; owner = "lass"; group = "syncthing"; }
  ];

  #networking.nameservers = [ "1.1.1.1" ];

  #time.timeZone = "Europe/Berlin";
}
