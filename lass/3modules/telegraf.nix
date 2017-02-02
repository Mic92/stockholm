{ config, lib, pkgs, ... }:

with builtins;
with lib;

let
  cfg = config.lass.telegraf;

  out = {
    options.lass.telegraf = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "telegraf";
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/telegraf";
    };
    user = mkOption {
      type = types.str;
      default = "telegraf";
    };
    outputs = mkOption {
      type = types.str;
      default = ''
        [outputs.influxdb]
          urls = ["http://localhost:8086"]
          database = "telegraf_db"
          user_agent = "telegraf"
      '';
    };
    inputs = mkOption {
      type = with types; listOf str;
      default = [
        ''
          [cpu]
            percpu = false
            totalcpu = true
            drop = ["cpu_time"]
        ''
      ];
    };
    interval = mkOption {
      type = types.str;
      default = "10s";
    };
    config = mkOption {
      type = types.str;
      #TODO: find a good default
      default = ''
        [agent]
            interval = "${cfg.interval}"

        [outputs]

        ${cfg.outputs}

        ${concatStringsSep "\n" cfg.inputs}

      '';
      description = "configuration telegraf is started with";
    };
  };

  configFile = pkgs.writeText "telegraf.conf" cfg.config;

  imp = {

    systemd.services.telegraf = {
      description = "telegraf";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      restartIfChanged = true;

      serviceConfig = {
        Restart = "always";
        ExecStart = "${pkgs.telegraf}/bin/telegraf -config ${configFile}";
      };
    };
  };

in out
