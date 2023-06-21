{ config, lib, pkgs, ... }:

with builtins;
with lib;

let
  cfg = config.krebs.kapacitor;

  out = {
    options.krebs.kapacitor = api;
    config = mkIf cfg.enable imp;
  };

  configOptions = recursiveUpdate {
    hostname = "localhost";
    data_dir = cfg.dataDir;
    http = {
      bind-address = ":9092";
      auth-enabled = false;
      log-enabled = false;
      gtgwrite-tracing = false;
      pprof-enabled = false;
      https-enabled = false;
      https-certificate = "/etc/ssl/kapacitor.pem";
      shutdown-timeout = "10s";
      shared-secret = "";
    };

    replay ={
      dir = "${cfg.dataDir}/replay";
    };

    storage = {
      boltdb = "${cfg.dataDir}/kapacitor.db";
    };

    task = {
      dir = "${cfg.dataDir}/tasks";
      snapshot-interval = "1m0s";
    };

    influxdb = [{
      enabled = true;
      name = "default";
      default = false;
      urls = ["http://localhost:8086"];
      username = "";
      password = "";
      ssl-ca = "";
      ssl-cert = "";
      ssl-key = "";
      insecure-skip-verify = false;
      timeout = "0s";
      disable-subscriptions = false;
      subscription-protocol = "http";
      udp-bind = "";
      udp-buffer = 1000;
      udp-read-buffer = 0;
      startup-timeout = "5m0s";
      subscriptions-sync-interval = "1m0s";
      influxdb.excluded-subscriptions = {
        _kapacitor = ["autogen"];
      };
    }];

    logging = {
      file = "STDERR";
      level = "INFO";
    };

    deadman = {
      interval = "10s";
      id = "{{ .Group }}:NODE_NAME for task '{{ .TaskName }}'";
      message = "{{ .ID }} is {{ if eq .Level \"OK\" }}alive{{ else }}dead{{ end }}: {{ index .Fields \"emitted\" | printf \"%0.3f\" }} points/INTERVAL.";
      global = false;
    };
  } cfg.extraConfig;

  api = {
    enable = mkEnableOption "kapacitor";
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/kapacitor";
    };
    user = mkOption {
      type = types.user;
      default = {
        name = "kapacitor";
        home = cfg.dataDir;
      };
    };
    group = mkOption {
      type = types.group;
      default = {
        name = "kapacitor";
      };
    };
    extraConfig = mkOption {
      type = types.attrs;
      default = {};
    };
    alarms = mkOption {
      type = with types; attrsOf (submodule {
        options = {
          database = mkOption {
            type = str;
          };
          text = mkOption {
            type = str;
          };
        };
      });
      default = {};
    };
  };

  configFile = pkgs.runCommand "kapacitor.toml" {} ''
    ${pkgs.remarshal}/bin/remarshal -if json -of toml \
      < ${pkgs.writeText "kapacitor.json" (builtins.toJSON configOptions)} \
      > $out
  '';

  imp = {
    users = {
      groups.${cfg.group.name} = {
        inherit (cfg.group) name gid;
      };
      users.${cfg.user.name} = {
        inherit (cfg.user) home name uid;
        createHome = true;
        group = cfg.group.name;
      };
    };

    systemd.services.kapacitor = {
      description = "kapacitor";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      restartIfChanged = true;

      serviceConfig = {
        Restart = "always";
        User = cfg.user.name;
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
        User = cfg.user.name;
        ExecStart = pkgs.writeDash "add_alarms" ''
          ${pkgs.kapacitor}/bin/kapacitor delete tasks \*
          ${concatStrings (mapAttrsToList (name: alarm: ''
            ${pkgs.kapacitor}/bin/kapacitor define ${name} \
              -type batch \
              -tick ${pkgs.writeText "${name}.tick" alarm.text} \
              -dbrp ${alarm.database}.default
            ${pkgs.kapacitor}/bin/kapacitor enable ${name}
          '') cfg.alarms)}
        '';
      };
    };

  };
in out
