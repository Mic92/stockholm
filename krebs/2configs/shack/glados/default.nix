{ config, pkgs, lib, ... }:
let
  kodi-host = "192.168.8.11";
  confdir = "/var/lib/homeassistant-docker";
in {
  imports = [ ./zigbee.nix ];

  networking.firewall.allowedTCPPorts = [ 8123 ];
  virtualisation.oci-containers.containers.hass = {
    image = "homeassistant/home-assistant:latest";
    environment = {
      TZ = "Europe/Berlin";
      # TODO create unique users
      PUID = toString config.users.users.news_container.uid;
      PGID = toString config.users.groups.news_container.gid;
      UMASK = "007";
    };
    extraOptions = ["--net=host" "--device=/dev/zigbee" ];
    volumes = [
      "${confdir}:/config"
      "${./zigbee-quirks}:/quirks"
      #"${confdir}/docker-run:/etc/services.d/home-assistant/run:"
    ];
  };
  systemd.tmpfiles.rules = [
    #"f ${confdir}/docker-run 0770 kiosk kiosk - -"
    # TODO:
    "d ${confdir} 0770 news_container news_container - -"
  ];

  services.nginx.virtualHosts."hass.shack" = {
    serverAliases = [ "glados.shack" ];
    locations."/" = {
      proxyPass = "http://localhost:8123";
      extraConfig = ''
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "upgrade";
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header Host             $host;
          proxy_set_header X-Real-IP        $remote_addr;

          proxy_buffering off;
        '';
    };
  };
}
