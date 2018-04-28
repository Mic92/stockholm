with import <stockholm/lib>;
{ config, lib, pkgs, ... }:
let
  inherit (import <stockholm/lass/2configs/websites/util.nix> {inherit lib pkgs;})
    servephpBB
  ;
in
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/websites>
    <stockholm/lass/2configs/websites/sqlBackup.nix>
    (servephpBB [ "rote-allez-fraktion.de" ])
  ];

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 80"; target = "ACCEPT"; }
  ];

  krebs.build.host = config.krebs.hosts.red;
  boot.isContainer = true;
  networking.useDHCP = false;

  services.nginx.enable = true;
  environment.systemPackages = [
    pkgs.mk_sql_pair
  ];
}
