{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  services.dnscrypt-proxy = {
    enable = true;
    localAddress = "127.1.0.1";
    customResolver = {
      address = config.krebs.hosts.gum.nets.internet.ip4.addr;
      port = 15251;
      name = "2.dnscrypt-cert.euer.krebsco.de";
      key = "1AFC:E58D:F242:0FBB:9EE9:4E51:47F4:5373:D9AE:C2AB:DD96:8448:333D:5D79:272C:A44C";
    };
  };
  services.dnsmasq = {
    enable = true;
    resolveLocalQueries = false;
    extraConfig = ''
      server=127.1.0.1
      #no-resolv
      cache-size=1000
      min-cache-ttl=3600
      bind-dynamic
      all-servers
      dnssec
      trust-anchor=.,19036,8,2,49AAC11D7B6F6446702E54A1607371607A1A41855200FD2CE1CDDE32F24E8FB5
      rebind-domain-ok=/onion/
      server=/.onion/127.0.0.1#9053
      port=53
    '';
  };
  networking.extraResolvconfConf = ''
    name_servers='127.0.0.1'
  '';
}
