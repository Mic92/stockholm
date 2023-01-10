{ config, pkgs, ... }:
with import <stockholm/lib>;

{
  systemd.network.networks."50-et0" = {
    matchConfig.Name = "et0";
    DHCP = "yes";
    # dhcpV4Config.UseDNS = false;
    # dhcpV6Config.UseDNS = false;
    linkConfig = {
      RequiredForOnline = "routable";
    };
    # networkConfig = {
    #   LinkLocalAddressing = "no";
    # };
    # dhcpV6Config = {
    #   PrefixDelegationHint = "::/60";
    # };
    # networkConfig = {
    #   IPv6AcceptRA = true;
    # };
    # ipv6PrefixDelegationConfig = {
    #   Managed = true;
    # };
  };
  systemd.network.networks."50-int0" = {
    name = "int0";
    address = [
      "10.42.0.1/24"
    ];
    networkConfig = {
      IPForward = "yes";
      IPMasquerade = "both";
      ConfigureWithoutCarrier = true;
      DHCPServer = "yes";
      # IPv6SendRA = "yes";
      # DHCPPrefixDelegation = "yes";
    };
  };
  networking.networkmanager.unmanaged = [ "int0" ];
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i int0"; target = "ACCEPT"; }
  ];
  krebs.iptables.tables.filter.FORWARD.rules = [
    { predicate = "-i int0"; target = "ACCEPT"; }
    { predicate = "-o int0"; target = "ACCEPT"; }
    { predicate = "-p ipv6-icmp"; target = "ACCEPT"; v4 = false; }
  ];
  krebs.iptables.tables.nat.PREROUTING.rules = mkBefore [
    { v6 = false; predicate = "-s 10.42.0.0/24"; target = "ACCEPT"; }
  ];

  networking.domain = "gg23";

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
}
