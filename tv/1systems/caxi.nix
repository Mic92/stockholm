{ config, ... }:

with import <stockholm/lib>;

{
  krebs.build.host = config.krebs.hosts.caxi;

  imports = [
    ../.
    ../2configs/hw/CAC-Developer-1.nix
    ../2configs/fs/CAC-CentOS-7-64bit.nix
    ../2configs/retiolum.nix
  ];

  networking = let
    inherit (config.krebs.build.host.nets.internet) ip4;
  in {
    interfaces.enp2s1.ip4 = singleton {
      address = ip4.addr;
      prefixLength = fromJSON (head (match ".*/([0-9]+)" ip4.prefix));
    };
    defaultGateway = head (match "([^/]*)\.0/[0-9]+" ip4.prefix) + ".1";
    nameservers = ["8.8.8.8"];
  };
}
