{ config, lib, pkgs, ... }:
{
  services.murmur = {
    enable = true;
    bandwidth = 10000000;
    registerName = "lassul.us";
    autobanTime = 30;
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 64738"; target = "ACCEPT";}
    { predicate = "-p udp --dport 64738"; target = "ACCEPT";}
  ];

  systemd.services.docker-mumble-web.serviceConfig = {
    StandardOutput = lib.mkForce "journal";
    StandardError = lib.mkForce "journal";
  };
  virtualisation.oci-containers.containers.mumble-web = {
    image = "rankenstein/mumble-web";
    environment = {
      MUMBLE_SERVER = "lassul.us:64738";
    };
    ports = [
      "64739:8080"
    ];
  };

  services.nginx.virtualHosts."mumble.lassul.us" = {
    enableACME = true;
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:64739/;
      proxy_set_header Accept-Encoding "";
      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection $connection_upgrade;
    '';
  };
}
