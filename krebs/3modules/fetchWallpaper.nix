{ config, lib, pkgs, ... }:

with config.krebs.lib;

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
      default = ":11";
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

  fetchWallpaperScript = pkgs.writeScript "fetchWallpaper" ''
    #! ${pkgs.bash}/bin/bash

    mkdir -p ${shell.escape cfg.stateDir}
    curl -s -o ${shell.escape cfg.stateDir}/wallpaper -z ${shell.escape cfg.stateDir}/wallpaper ${shell.escape cfg.url}
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
