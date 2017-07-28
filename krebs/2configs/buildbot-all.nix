{ lib, config, pkgs, ... }:
{
  imports = [
    <stockholm/krebs/2configs/repo-sync.nix>
  ];

  networking.firewall.allowedTCPPorts = [ 80 8010 9989 ];
  krebs.ci.enable = true;
  krebs.ci.users.krebs.all = true;
  krebs.ci.users.lass.all = true;
  krebs.ci.users.makefu.all = true;
  krebs.ci.users.nin.all = true;
  krebs.ci.users.tv.all = true;
}

