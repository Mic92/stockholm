{ config, lib, pkgs, ... }:

with config.krebs.lib;

let
  cfg = config.krebs.power-action;

  out = {
    options.krebs.power-action = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "power-action";
    battery = mkOption {
      type = types.str;
      default = "BAT0";
    };
    user = mkOption {
      type = types.user;
      default = {
        name = "power-action";
      };
    };
    startAt = mkOption {
      type = types.str;
      default = "*:0/1";
    };
    plans = mkOption {
      type = with types; attrsOf (submodule {
        options = {
          charging = mkOption {
            type = nullOr bool;
            default = null;
            description = ''
              check for charging status.
              null = don't care
              true = only if system is charging
              false = only if system is discharging
            '';
          };
          upperLimit = mkOption {
            type = int;
          };
          lowerLimit = mkOption {
            type = int;
          };
          action = mkOption {
            type = path;
          };
        };
      });
    };
  };

  imp = {
    systemd.services.power-action = {
      serviceConfig = rec {
        ExecStart = startScript;
        User = cfg.user.name;
      };
      startAt = cfg.startAt;
    };
    users.users.${cfg.user.name} = {
      inherit (cfg.user) name uid;
    };
  };

  startScript = pkgs.writeDash "power-action" ''
    set -euf

    power="$(${powerlvl})"
    state="$(${state})"
    ${concatStringsSep "\n" (mapAttrsToList writeRule cfg.plans)}
  '';
  charging_check = plan:
    if (plan.charging == null) then "" else
      if plan.charging
        then ''&& [ "$state" = "true" ]''
        else ''&& ! [ "$state" = "true" ]''
    ;

  writeRule = _: plan:
    "if [ $power -ge ${toString plan.lowerLimit} ] && [ $power -le ${toString plan.upperLimit} ] ${charging_check plan}; then ${plan.action}; fi";

  powerlvl = pkgs.writeDash "powerlvl" ''
    cat /sys/class/power_supply/${cfg.battery}/capacity
  '';

  state = pkgs.writeDash "state" ''
    if [ "$(cat /sys/class/power_supply/${cfg.battery}/status)" = "Discharging" ]
      then echo "false"
      else echo "true"
    fi
  '';

in out
