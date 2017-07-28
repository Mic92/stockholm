with import <stockholm/lib>;
{ config, pkgs, ... }: let

  bestGuessGateway = addr: elemAt (match "(.*)(\.[^.])" addr) 0 + ".1";

in {
  krebs.build.host = config.krebs.hosts.cd;

  imports = [
    <stockholm/tv>
    <stockholm/tv/2configs/hw/CAC-Developer-2.nix>
    <stockholm/tv/2configs/fs/CAC-CentOS-7-64bit.nix>
    <stockholm/tv/2configs/exim-smarthost.nix>
    <stockholm/tv/2configs/retiolum.nix>
  ];

  networking = let
    address = config.krebs.build.host.nets.internet.ip4.addr;
  in {
    defaultGateway = bestGuessGateway address;
    interfaces.enp2s1.ip4 = singleton {
      inherit address;
      prefixLength = 24;
    };
    nameservers = ["8.8.8.8"];
  };

  environment.systemPackages = with pkgs; [
    iftop
    iotop
    iptables
    nethogs
    tcpdump
  ];
}
