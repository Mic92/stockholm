{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  services.dnscrypt-proxy = {
    enable = true;
    localAddress = "127.1.0.1";
    resolverName = "cs-de";
  };
  services.dnsmasq = {
    enable = true;
    extraConfig = ''
      server=127.1.0.1
      server=/dn42/172.23.75.6
      #no-resolv
      cache-size=1000
      min-cache-ttl=3600
      bind-dynamic
      all-servers
      dnssec
      trust-anchor=.,19036,8,2,49AAC11D7B6F6446702E54A1607371607A1A41855200FD2CE1CDDE32F24E8FB5
      address=/blog/127.0.0.1
      address=/blog/::1
      rebind-domain-ok=/onion/
      server=/.onion/127.0.0.1#9053
      port=53
    '';
  };
  networking.extraResolvconfConf = ''
    name_servers='127.0.0.1'
  '';
}
