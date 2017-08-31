with import <stockholm/lib>;
{ lib, config, pkgs, ... }:
{
  imports = [
    <stockholm/krebs/2configs/repo-sync.nix>
  ];

  networking.firewall.allowedTCPPorts = [ 80 8010 9989 ];
  krebs.ci.enable = true;
  krebs.ci.treeStableTimer = 120;
  krebs.ci.hosts = [ config.krebs.build.host ];
}
