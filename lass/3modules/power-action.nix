{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.lass.power-action;

  out = {
    options.lass.power-action = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "power-action";
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
        User = cfg.user;
      };
      startAt = cfg.startAt;
    };
    users.users.${cfg.user.name} = {
      inherit (cfg.user) name uid;
    };
  };

  startScript = pkgs.writeDash "power-action" ''
    power="$(${powerlvl})"
    ${concatStringsSep "\n" (mapAttrsToList writeRule cfg.plans)}
  '';

  writeRule = _: plan:
    "if [ $power -ge ${toString plan.lowerLimit} ] && [ $power -le ${toString plan.upperLimit} ]; then ${plan.action}; fi";

  powerlvl = pkgs.writeDash "powerlvl" ''
    cat /sys/class/power_supply/BAT0/capacity
  '';

in out
