{ config, pkgs, ... }:
with import <stockholm/lib>;

{
  networking.networkmanager.unmanaged = [ "int0" ];
  networking.interfaces.int0.ipv4.addresses = [{
    address = "10.42.0.1";
    prefixLength = 24;
  }];

  networking.domain = "gg23";

  services.dhcpd4 = {
    enable = true;
    interfaces = [ "int0" ];
    extraConfig = ''
      option subnet-mask 255.255.255.0;
      option routers 10.42.0.1;
      option domain-name-servers 10.42.0.1;
      subnet 10.42.0.0 netmask 255.255.255.0 {
        range 10.42.0.100 10.42.0.200;
      }
    '';
    machines = [
      { ethernetAddress = "a8:a6:48:65:ce:4c"; hostName = "tv"; ipAddress = "10.42.0.3"; }
      { ethernetAddress = "3c:2a:f4:22:28:37"; hostName = "drucker"; ipAddress = "10.42.0.4"; }
      { ethernetAddress = "80:7d:3a:67:b7:01"; hostName = "s20-tv"; ipAddress = "10.42.0.10"; }
      { ethernetAddress = "80:7d:3a:68:04:f0"; hostName = "s20-drucker"; ipAddress = "10.42.0.11"; }
      { ethernetAddress = "80:7d:3a:68:11:a5"; hostName = "s20-wasch"; ipAddress = "10.42.0.12"; }
      { ethernetAddress = "80:7d:3a:67:bb:69"; hostName = "s20-stereo"; ipAddress = "10.42.0.13"; }
      { ethernetAddress = "ec:b5:fa:07:78:16"; hostName = "hue-bridge"; ipAddress = "10.42.0.21"; }
      { ethernetAddress = "80:8d:b7:c5:80:dc"; hostName = "arubaAP"; ipAddress = "10.42.0.99"; }
    ];
  };

  services.dnsmasq = {
    enable = true;
    resolveLocalQueries = false;

    extraConfig = ''
      local=/gg23/
      domain=gg23
      expand-hosts
      listen-address=10.42.0.1
      interface=int0
    '';
  };

  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i int0 -p udp --dport 53"; target = "ACCEPT"; } # dns
  ];
  krebs.iptables.tables.filter.FORWARD.rules = [
    { v6 = false; predicate = "-d 10.42.0.0/24 -o int0 -m conntrack --ctstate RELATED,ESTABLISHED"; target = "ACCEPT"; }
    { v6 = false; predicate = "-s 10.42.0.0/24 -i int0"; target = "ACCEPT"; }
    { v6 = false; predicate = "-o int0"; target = "REJECT --reject-with icmp-port-unreachable"; }
    { v6 = false; predicate = "-i int0"; target = "REJECT --reject-with icmp-port-unreachable"; }
  ];
  krebs.iptables.tables.nat.PREROUTING.rules = mkBefore [
    { v6 = false; predicate = "-s 10.42.0.0/24"; target = "ACCEPT"; }
  ];
  krebs.iptables.tables.nat.POSTROUTING.rules = [
    { v6 = false; predicate = "-s 10.42.0.0/24 ! -d 10.42.0.0/24"; target = "MASQUERADE"; }
  ];
}

