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
  krebs.pages.nginx.useACMEHost = "krebsco.de";

  security.acme.acceptTerms = true;
  security.acme.certs."krebsco.de" = {
    domain = "krebsco.de";
    extraDomainNames = [
      "*.krebsco.de"
    ];
    email = "spam@krebsco.de";
    reloadServices = [
      "knsupdate-krebsco.de.service"
      "nginx.service"
    ];
    keyType = "ec384";
    dnsProvider = "rfc2136";
    credentialsFile = "/var/src/secrets/acme-credentials";
  };

  users.users.nginx.extraGroups = [ "acme" ];
}
