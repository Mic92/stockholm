{ pkgs, config, ... }:

with import <stockholm/lib>;

let
  cfg = config.lass.screenlock;

  out = {
    options.lass.screenlock = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "news";
    command = mkOption {
      type = types.str;
      default = "${pkgs.i3lock}/bin/i3lock -i /var/lib/wallpaper/wallpaper -f";
    };
  };

  imp = {
    systemd.services.screenlock = {
      before = [ "sleep.target" ];
      wantedBy = [ "sleep.target" ];
      environment = {
        DISPLAY = ":0";
      };
      serviceConfig = {
        SyslogIdentifier = "screenlock";
        ExecStart = cfg.command;
        Type = "forking";
        User = "lass";
      };
    };
  };

in out
