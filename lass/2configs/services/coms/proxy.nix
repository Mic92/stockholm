{ config, lib, pkgs, ... }:
let
  tcpports = [
    4443 # jitsi
    64738 # murmur
  ];
  udpports = [
    10000 # jitsi
    64738 # murmur
  ];
  target = "orange.r";
in
{
  networking.firewall.allowedTCPPorts = tcpports;
  networking.firewall.allowedUDPPorts = udpports;
  services.nginx.streamConfig = ''
    ${lib.concatMapStringsSep "\n" (port: ''
      server {
        listen ${toString port};
        proxy_pass ${target}:${toString port};
      }
    '') tcpports}
    ${lib.concatMapStringsSep "\n" (port: ''
      server {
        listen ${toString port} udp;
        proxy_pass ${target}:${toString port};
      }
    '') udpports}
  '';

  services.nginx.virtualHosts."jitsi.lassul.us" = {
    enableACME = true;
    acmeFallbackHost = "${target}";
    addSSL = true;
    locations."/" = {
      recommendedProxySettings = true;
      proxyWebsockets = true;
      proxyPass = "http://${target}";
    };
  };
}
