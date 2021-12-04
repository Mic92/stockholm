{ config, lib, pkgs, ... }:

let
  pkg =
    pkgs.fetchFromGitHub {
      owner = "shackspace";
      repo = "shackdns";
      rev = "e55cc906c734b398683f9607b93f1ad6435d8575";
      sha256 = "1hkwhf3hqb4fz06b1ckh7sl0zcyi4da5fgdlksian8lxyd19n8sq";
	  };
  home = "/var/lib/shackDNS";
  port = "8083";
  config_file = pkgs.writeText "config" ''
  # Points to a bind configuration file
  dns-db = ${home}/db.shack

  # Points to a shackles configuration file
  # See `shackles.json` in repo
  shackles-db = ${home}/shackles.json

  # Points to a REST service with the DHCP leases
  leases-api = http://dhcp.shack/dhcpd.leases

  # Wrap this binding with https proxy or similar
  binding = http://localhost:${port}/
  '';
in {
  # receive response from light.shack / standby.shack
  networking.firewall.allowedTCPPorts = [ ];

  users.users.shackDNS = {
    inherit home;
    group = "nogroup";
    createHome = true;
    isSystemUser = true;
  };
  users.groups.shackDNS = {};
  services.nginx.virtualHosts."leases.shack" = {
    locations."/" = {
      proxyPass = "http://localhost:${port}/";
    };
  };
  services.nginx.virtualHosts."shackdns.shack" = {
    locations."/" = {
      proxyPass = "http://localhost:${port}/";
    };
  };
  services.nginx.virtualHosts."shackles.shack" = {
    locations."/" = {
      proxyPass = "http://localhost:${port}/";
    };
  };

  systemd.services.shackDNS = {
    description = "shackDNS provides an overview over DHCP and DNS as well as a replacement for shackles";
    wantedBy = [ "multi-user.target" ];
    environment.PORT = port;
    serviceConfig = {
      User = "shackDNS";
      WorkingDirectory = home;
      ExecStart = "${pkgs.mono6}/bin/mono ${pkg}/shackDNS.exe ${config_file}";
      PrivateTmp = true;
      Restart = "always";
      RestartSec = "15";
    };
  };
}
