{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  cfg = config.krebs.fetchWallpaper;

  out = {
    options.krebs.fetchWallpaper = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "fetch wallpaper";
    url = mkOption {
      type = types.str;
    };
    timerConfig = mkOption {
      type = types.unspecified;
      default = {
        OnCalendar = "*:00,10,20,30,40,50";
      };
    };
    stateDir = mkOption {
      type = types.str;
      default = "/var/lib/wallpaper";
    };
    display = mkOption {
      type = types.str;
      default = ":${toString config.services.xserver.display}";
    };
    unitConfig = mkOption {
      type = types.attrsOf types.str;
      description = "Extra unit configuration for fetchWallpaper to define conditions and assertions for the unit";
      example = literalExample ''
        # do not start when running on umts
        { ConditionPathExists = "!/var/run/ppp0.pid"; }
      '';
      default = {};
    };
  };

  fetchWallpaperScript = pkgs.writeDash "fetchWallpaper" ''
    set -euf

    mkdir -p ${cfg.stateDir}
    chmod o+rx ${cfg.stateDir}
    cd ${cfg.stateDir}
    (curl -s -o wallpaper.tmp -z wallpaper.tmp ${shell.escape cfg.url} && cp wallpaper.tmp wallpaper) || :
    feh --no-fehbg --bg-scale wallpaper
  '';

  imp = {
    users.users.fetchWallpaper = {
      name = "fetchWallpaper";
      uid = genid_uint31 "fetchWallpaper";
      description = "fetchWallpaper user";
      group = "fetchWallpaper";
      home = cfg.stateDir;
      createHome = true;
      isSystemUser = true;
    };
    users.groups.fetchWallpaper = {};

    systemd.timers.fetchWallpaper = {
      description = "fetch wallpaper timer";
      wantedBy = [ "timers.target" ];

      timerConfig = cfg.timerConfig;
    };
    systemd.services.fetchWallpaper = {
      description = "fetch wallpaper";
      after = [ "network.target" ];

      path = with pkgs; [
        curl
        feh
      ];

      environment = {
        URL = cfg.url;
        DISPLAY = cfg.display;
      };
      restartIfChanged = true;

      serviceConfig = {
        Type = "simple";
        ExecStart = fetchWallpaperScript;
        User = "fetchWallpaper";
      };

      unitConfig = cfg.unitConfig;
    };
  };
in out
