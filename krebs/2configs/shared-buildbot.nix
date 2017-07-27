{ lib, config, pkgs, ... }:
{
  imports = [
    <stockholm/krebs/2configs/repo-sync.nix>
  ];

  networking.firewall.allowedTCPPorts = [ 80 8010 9989 ];
  krebs.ci.enable = true;
  krebs.ci.users.krebs ={
    all = true;
    hosts = [
      "test-arch"
      "test-centos6"
      "test-centos7"
      "test-all-krebs-modules"
    ];
  };
}
