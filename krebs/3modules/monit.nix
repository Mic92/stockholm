{ config, lib, pkgs, ... }:

with builtins;
with lib;

let
  cfg = config.krebs.monit;

  out = {
    options.krebs.monit = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "monit";
    http = {
      enable = mkEnableOption "monit http server";
      port = mkOption {
        type = types.int;
        default = 9093;
      };
      user = mkOption {
        type = types.str;
        default = "krebs";
      };
      pass = mkOption {
        type = types.str;
        default = "bob";
      };
    };
    user = mkOption {
      type = types.user;
      default = {
        name = "monit";
      };
    };
    group = mkOption {
      type = types.group;
      default = {
        name = "monitor";
      };
    };
    extraConfig = mkOption {
      type = types.attrs;
      default = {};
    };
    alarms = mkOption {
      default = {};
      type = with types; attrsOf (submodule {
        options = {
          test = mkOption {
            type = either path str;
          };
          alarm = mkOption {
            type = either path str;
          };
          interval = mkOption {
            type = str;
            default = "10";
          };
        };
      });
    };
  };

  imp = let
    configFile = pkgs.writeText "monit.cfg" ''
      ${optionalString cfg.http.enable ''
        set httpd port ${toString cfg.http.port}
          allow ${cfg.http.user}:${cfg.http.pass}
      ''}
      set daemon 10

      ${concatStringsSep "\n" (mapAttrsToList (name: alarm: ''
        check program ${name} with path "${alarm.test}"
          every ${alarm.interval} cycles
          if status != 0 then exec "${alarm.alarm}"
      '') cfg.alarms)}
    '';
  in {
    environment.etc = [
      {
        source = configFile;
        target = "monit.conf";
        mode = "0400";
        uid = config.users.users.${cfg.user.name}.uid;
      }
    ];
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

    systemd.services.monit = {
      description = "monit";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      restartIfChanged = true;

      serviceConfig = {
        Restart = "always";
        User = cfg.user.name;
        ExecStart = "${pkgs.monit}/bin/monit -I -c /etc/monit.conf";
        # Monit should restart when the config changes
        ExecStartPre = "${pkgs.coreutils}/bin/echo ${configFile}";
      };
    };
  };
in out
