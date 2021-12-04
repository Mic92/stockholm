{ config, pkgs, lib, ... }:
let
  unstable = import (pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = (lib.importJSON ../../../nixpkgs-unstable.json).rev;
    sha256 = (lib.importJSON ../../../nixpkgs-unstable.json).sha256;
  }) {};
in {
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
  imports = [
    ./multi/shackopen.nix
    ./multi/wasser.nix
    ./multi/schlechte_luft.nix
    ./multi/rollos.nix

    ./switch/power.nix

    ./sensors/power.nix
    ./sensors/mate.nix
    ./sensors/darksky.nix
    ./sensors/spaceapi.nix
    ./sensors/sensemap.nix

    ./automation/shack-startup.nix
    ./automation/party-time.nix
    ./automation/hass-restart.nix
    ./automation/ampel.nix

  ];
  services.home-assistant =
    {
    enable = true;
    autoExtraComponents = true;
    package = unstable.home-assistant.overrideAttrs (old: {
      doInstallCheck = false;
    });
    config = {
      homeassistant = {
        name = "Glados";
        time_zone = "Europe/Berlin";
        latitude = "48.8265";
        longitude = "9.0676";
        elevation = 303;
        auth_providers = [
          { type = "homeassistant";}
          { type = "trusted_networks";
            trusted_networks = [
              "127.0.0.1/32"
              "10.42.0.0/16"
              "::1/128"
              "fd00::/8"
            ];
          }
        ];
      };
      # https://www.home-assistant.io/components/influxdb/
      influxdb = {
        database = "glados";
        host = "influx.shack";
        component_config_glob = {
          "sensor.*particulate_matter_2_5um_concentration".override_measurement = "2_5um particles";
          "sensor.*particulate_matter_10_0um_concentration".override_measurement ="10um particles";
        };
        tags = {
          instance = "wolf";
          source = "glados";
        };
      };
      esphome = {};
      api = {};
      mqtt = {
        broker = "localhost";
        port = 1883;
        client_id = "home-assistant";
        keepalive = 60;
        protocol = 3.1;
        discovery = true; #enable esphome discovery
        discovery_prefix = "homeassistant";
        birth_message = {
          topic = "glados/hass/status/LWT";
          payload = "Online";
          qos = 1;
          retain = true;
        };
        will_message = {
          topic = "glados/hass/status/LWT";
          payload = "Offline";
          qos = 1;
          retain = true;
        };
      };
      light =  [];
      media_player = [
        { platform = "mpd";
          name = "lounge";
          host = "lounge.mpd.shack";
        }
        { platform = "mpd";
          name = "kiosk";
          #host = "lounge.kiosk.shack";
          host = "kiosk.shack";
        }
      ];

      camera = [];
      frontend = { };
      config = { };
      sun = {};
      http = {
        base_url = "http://hass.shack";
        use_x_forwarded_for = true;
        trusted_proxies = [ "127.0.0.1" "::1" ];
      };
      #conversation = {};

      history = {};
      logbook = {};
      #recorder = {};

      logger.default = "info";

      tts = [
        { platform = "google_translate";
          service_name = "say";
          language = "de";
          cache = true;
          time_memory = 57600;
          base_url = "http://hass.shack";
        }
      ];
      device_tracker = [];
    };
  };
}
