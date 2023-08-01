{ config, pkgs, ... }:
{
  imports = [
    ./hw.nix
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    <stockholm/krebs/2configs/matterbridge.nix>
    <stockholm/krebs/2configs/nameserver.nix>
  ];

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.firewall.logRefusedConnections = false;
  networking.firewall.logRefusedUnicastsOnly = false;

  # Move Internet-facing SSH port to reduce logspam.
  networking.firewall.extraCommands = let
    host = config.krebs.build.host;
  in /* sh */ ''
    iptables -t nat -A OUTPUT -o lo -p tcp --dport 11423 -j REDIRECT --to-ports 22
    iptables -t nat -A PREROUTING -p tcp --dport 11423 -j REDIRECT --to-ports 22
    iptables -t nat -A PREROUTING -d ${host.nets.retiolum.ip4.addr} -p tcp --dport 22 -j ACCEPT
    iptables -t nat -A PREROUTING -p tcp --dport 22 -j REDIRECT --to-ports 0

    ip6tables -t nat -A OUTPUT -o lo -p tcp --dport 11423 -j REDIRECT --to-ports 22
    ip6tables -t nat -A PREROUTING -p tcp --dport 11423 -j REDIRECT --to-ports 22
    ip6tables -t nat -A PREROUTING -d ${host.nets.retiolum.ip6.addr} -p tcp --dport 22 -j ACCEPT
    ip6tables -t nat -A PREROUTING -p tcp --dport 22 -j REDIRECT --to-ports 0
  '';

  krebs.build.host = config.krebs.hosts.ponte;

  krebs.pages.enable = true;
  krebs.pages.nginx.addSSL = true;
  krebs.pages.nginx.enableACME = true;

  security.acme.acceptTerms = true;
  security.acme.certs.${config.krebs.pages.domain}.email = "spam@krebsco.de";
}
