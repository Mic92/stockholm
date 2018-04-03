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
      type = types.str;
      default = "${pkgs.xlockmore}/bin/xlock -mode life1d -size 1";
    };
  };

  imp = {
    systemd.services.screenlock = {
      before = [ "sleep.target" ];
      wantedBy = [ "sleep.target" ];
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
