{ config, lib, pkgs, ... }:

with builtins;
with lib;

let
  cfg = config.lass.kapacitor;

  out = {
    options.lass.kapacitor = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "kapacitor";
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/kapacitor";
    };
    user = mkOption {
      type = types.str;
      default = "kapacitor";
    };
    logLevel = mkOption {
      type = types.enum ["DEBUG" "INFO" "WARN" "ERROR" "OFF"];
      default = "INFO";
    };
    alarms = mkOption {
      type = with types; attrsOf str;
      default = {};
    };
    check_db = mkOption {
      type = types.str;
      default = "all_data";
    };
    config = mkOption {
      type = types.str;
      #TODO: find a good default
      default = ''
        hostname = "localhost"
        data_dir = "${cfg.dataDir}"

        [http]
          bind-address = ":9092"
          auth-enabled = false
          log-enabled = true
          write-tracing = false
          pprof-enabled = false
          https-enabled = false
          https-certificate = "/etc/ssl/kapacitor.pem"
          shutdown-timeout = "10s"
          shared-secret = ""

        [replay]
          dir = "${cfg.dataDir}/replay"

        [storage]
          boltdb = "${cfg.dataDir}/kapacitor.db"

        [task]
          dir = "${cfg.dataDir}/tasks"
          snapshot-interval = "1m0s"

        [[influxdb]]
          enabled = true
          name = "default"
          default = false
          urls = ["http://localhost:8086"]
          username = ""
          password = ""
          ssl-ca = ""
          ssl-cert = ""
          ssl-key = ""
          insecure-skip-verify = false
          timeout = "0s"
          disable-subscriptions = false
          subscription-protocol = "http"
          udp-bind = ""
          udp-buffer = 1000
          udp-read-buffer = 0
          startup-timeout = "5m0s"
          subscriptions-sync-interval = "1m0s"
          [influxdb.subscriptions]
          [influxdb.excluded-subscriptions]
            _kapacitor = ["autogen"]

        [logging]
          file = "STDERR"
          level = "${cfg.logLevel}"

        [deadman]
          interval = "10s"
          threshold = 0.0
          id = "{{ .Group }}:NODE_NAME for task '{{ .TaskName }}'"
          message = "{{ .ID }} is {{ if eq .Level \"OK\" }}alive{{ else }}dead{{ end }}: {{ index .Fields \"emitted\" | printf \"%0.3f\" }} points/INTERVAL."
          global = false
      '';
      description = "configuration kapacitor is started with";
    };
  };

  configFile = pkgs.writeText "kapacitor.conf" cfg.config;

  imp = {

    systemd.services.kapacitor = {
      description = "kapacitor";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      restartIfChanged = true;

      serviceConfig = {
        Restart = "always";
        ExecStart = "${pkgs.kapacitor}/bin/kapacitord -config ${configFile}";
      };
    };

    systemd.services.kapacitor-alarms = {
      description = "kapacitor-alarms";
      after = [ "kapacitor.service" ];
      wantedBy = [ "multi-user.target" ];

      restartIfChanged = true;

      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeDash "add_alarms" ''
          ${pkgs.kapacitor}/bin/kapacitor delete tasks \*
          ${concatStrings (mapAttrsToList (name: alarm: ''
            ${pkgs.kapacitor}/bin/kapacitor define ${name} \
              -type batch \
              -tick ${pkgs.writeText "${name}.tick" alarm} \
              -dbrp ${cfg.check_db}.default
            ${pkgs.kapacitor}/bin/kapacitor enable ${name}
          '') cfg.alarms)}
        '';
      };
    };

  };

in out
