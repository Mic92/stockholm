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
    # TODO find a better default stateDir
    stateDir = mkOption {
      type = types.str;
      default = "./wallpaper";
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

    mkdir -p ${shell.escape cfg.stateDir}
    cd ${shell.escape cfg.stateDir}
    (curl --max-time ${toString cfg.maxTime} -s -o wallpaper.tmp -z wallpaper ${shell.escape cfg.url} && mv wallpaper.tmp wallpaper) || :
    feh --no-fehbg --bg-scale wallpaper
  '';

  imp = {
    systemd.user.timers.fetchWallpaper = {
      description = "fetch wallpaper timer";
      wantedBy = [ "timers.target" ];

      timerConfig = cfg.timerConfig;
    };
    systemd.user.services.fetchWallpaper = {
      description = "fetch wallpaper";
      after = [ "network.target" "graphical.target" ];
      wants = [ "graphical.target" ];
      wantedBy = [ "default.target" ];

      path = with pkgs; [
        curl
        feh
        coreutils
      ];

      environment = {
        DISPLAY = cfg.display;
      };
      restartIfChanged = true;

      serviceConfig = {
        Type = "simple";
        ExecStart = fetchWallpaperScript;
      };

      unitConfig = cfg.unitConfig;
    };
  };
in out
