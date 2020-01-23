{ config, pkgs, lib, ... }:
let
  shackopen = import ./multi/shackopen.nix;
  wasser = import ./multi/wasser.nix;
  badair = import ./multi/schlechte_luft.nix;
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
  services.home-assistant = let
      dwd_pollen = pkgs.fetchFromGitHub {
        owner = "marcschumacher";
        repo = "dwd_pollen";
        rev = "0.1";
        sha256 = "1af2mx99gv2hk1ad53g21fwkdfdbymqcdl3jvzd1yg7dgxlkhbj1";
      };
    in {
    enable = true;
    package = (pkgs.home-assistant.overrideAttrs (old: {
      # TODO: find correct python package
      installCheckPhase = ''
        echo LOLLLLLLLLLLLLLL
      '';
      postInstall = ''
        cp -r ${dwd_pollen} $out/lib/python3.7/site-packages/homeassistant/components/dwd_pollen
      '';
    })).override {
      extraPackages = ps: with ps; [
        python-forecastio jsonrpc-async jsonrpc-websocket mpd2
      ];
    };
    autoExtraComponents = true;
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
      #influxdb = {
      #  database = "hass";
      #  tags = {
      #    instance = "wolf";
      #    source = "hass";
      #  };
      #};
      mqtt = {
        broker = "localhost";
        port = 1883;
        client_id = "home-assistant";
        keepalive = 60;
        protocol = 3.1;
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
      switch = wasser.switch;
      light =  badair.light;
      media_player = [
        { platform = "mpd";
          name = "lounge";
          host = "lounge.mpd.shack";
        }
      ];

      sensor =
           (import ./sensors/hass.nix)
        ++ (import ./sensors/power.nix)
        ++ shackopen.sensor
        ++ badair.sensor;

      binary_sensor = shackopen.binary_sensor;

      camera = [];

      frontend = { };
      config = { };
      http = {
        base_url = "http://hass.shack";
        use_x_forwarded_for = true;
        trusted_proxies = "127.0.0.1";
      };
      #conversation = {};
      history = {};
      logbook = {};
      logger = {
        default = "info";
      };
      recorder = {};
      tts = [
        { platform = "google_translate";
          service_name = "say";
          language = "de";
          cache = true;
          time_memory = 57600;
        }
        #{ platform = "picotts";
        #  language = "de-DE";
        #  service_name = "say";
        #}
      ];
      sun = {};

      automation = wasser.automation
        ++ badair.automation
        ++ (import ./automation/shack-startup.nix)
        ++ (import ./automation/hass-restart.nix);

      device_tracker = [];
    };
  };
}
