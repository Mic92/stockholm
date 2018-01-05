with import <stockholm/lib>;
{ lib, config, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [ 80 8010 9989 ];
  krebs.ci.enable = true;
  krebs.ci.treeStableTimer = 1;
  krebs.ci.hosts = filter (getAttr "ci") (attrValues config.krebs.hosts);
  krebs.ci.tests = [ "deploy" ];
}

