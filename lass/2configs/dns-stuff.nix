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
  services.resolved.enable = true;
  services.resolved.fallbackDns = [ "127.1.0.1" ];
}
