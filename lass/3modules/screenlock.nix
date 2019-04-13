{ pkgs, config, ... }:

with import <stockholm/lib>;

let
  cfg = config.lass.screenlock;

  out = {
    options.lass.screenlock = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "screenlock";
    command = mkOption {
      type = types.path;
      default = pkgs.writeDash "screenlock" ''
        ${pkgs.xlockmore}/bin/xlock -mode life1d -size 1
        sleep 3
      '';
    };
  };

  imp = {
    systemd.services.screenlock = {
      before = [ "sleep.target" ];
      requiredBy = [ "sleep.target" ];
      environment = {
        DISPLAY = ":${toString config.services.xserver.display}";
      };
      serviceConfig = {
        SyslogIdentifier = "screenlock";
        ExecStart = cfg.command;
        Type = "simple";
        User = "lass";
      };
    };
  };

in out
