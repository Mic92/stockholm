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
      default = ":0";
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
    maxTime = mkOption {
      type = types.int;
      default = 0;
      description = "Time to wait before download is aborted";
    };
  };

  fetchWallpaperScript = pkgs.writeDash "fetchWallpaper" ''
    set -euf

    mkdir -p ${cfg.stateDir}
    chmod o+rx ${cfg.stateDir}
    cd ${cfg.stateDir}
    (curl --max-time ${toString cfg.maxTime} -s -o wallpaper.tmp -z wallpaper ${shell.escape cfg.url} && mv wallpaper.tmp wallpaper) || :
    feh --no-fehbg --bg-scale ${shell.escape cfg.stateDir}/wallpaper
  '';

  imp = {
    users.users.fetchWallpaper = {
      name = "fetchWallpaper";
      uid = genid "fetchWallpaper";
      description = "fetchWallpaper user";
      home = cfg.stateDir;
      createHome = true;
    };

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
