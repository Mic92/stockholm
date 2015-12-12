{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.krebs.fetchWallpaper;

  out = {
    options.krebs.fetchWallpaper = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "fetch wallpaper";
    predicate = mkOption {
      type = with types; nullOr path;
      default = null;
    };
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
      default = "/tmp/wallpaper";
    };
    display = mkOption {
      type = types.str;
      default = ":11";
    };
  };

  fetchWallpaperScript = pkgs.writeScript "fetchWallpaper" ''
    #! ${pkgs.bash}/bin/bash
    ${if (cfg.predicate == null) then "" else ''
      ${cfg.predicate}
      if [ $? -ne 0 ]; then
        echo "predicate failed"
        exit 23
      fi
    ''}
    mkdir -p ${shell.escape cfg.stateDir}
    curl -s -o ${shell.escape cfg.stateDir}/wallpaper -z ${shell.escape cfg.stateDir}/wallpaper ${shell.escape cfg.url}
    feh --no-fehbg --bg-scale ${shell.escape cfg.stateDir}/wallpaper
  '';

  imp = {
    users.extraUsers.fetchWallpaper = {
      name = "fetchWallpaper";
      uid = 3332383611; #genid fetchWallpaper
      description = "fetchWallpaper user";
      home = "/var/empty";
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
    };
  };
in out
