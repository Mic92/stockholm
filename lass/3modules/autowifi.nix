{ config, lib, pkgs, ... }:
with import <stockholm/lib>;
let

  cfg = config.lass.autowifi;

in {
  options.lass.autowifi = {
    enable = mkEnableOption "automatic wifi connector";
    knownWifisFile = mkOption {
      type = types.str;
      default = "/etc/wifis";
    };
    enablePrisonBreak = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.autowifi = {
      description = "Automatic wifi connector";
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.networkmanager ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "10s";
        ExecStart = "${autowifi}/bin/autowifi";
      };
    };

    networking.networkmanager.dispatcherScripts = mkIf cfg.enablePrisonBreak [
      { source = "${pkgs.callPackage <stockholm/makefu/5pkgs/prison-break}/bin/prison-break"; }
    ];
  };
}

