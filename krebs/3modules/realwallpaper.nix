{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  cfg = config.krebs.realwallpaper;

  out = {
    options.krebs.realwallpaper = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "realwallpaper";

    workingDir = mkOption {
      type = types.str;
      default = "/var/realwallpaper/";
    };

    nightmap = mkOption {
      type = types.str;
      default = "http://eoimages.gsfc.nasa.gov/images/imagerecords/55000/55167/earth_lights_lrg.jpg";
    };

    daymap = mkOption {
      type = types.str;
      default = "https://www.nnvl.noaa.gov/images/globaldata/SnowIceCover_Daily.png";
    };

    cloudmap = mkOption {
      type = types.str;
      default = "http://xplanetclouds.com/free/local/clouds_2048.jpg";
    };

    outFile = mkOption {
      type = types.str;
      default = "/tmp/wallpaper.png";
    };

    timerConfig = mkOption {
      type = types.unspecified;
      default = {
        OnCalendar = "*:0/15";
      };
    };

  };

  imp = {
    systemd.timers.realwallpaper = {
      description = "real wallpaper generator timer";
      wantedBy = [ "timers.target" ];

      timerConfig = cfg.timerConfig;
    };

    systemd.services.realwallpaper = {
      description = "real wallpaper generator";
      after = [ "network.target" ];

      path = with pkgs; [
        xplanet
        imagemagick
        curl
        file
      ];

      environment = {
        working_dir = cfg.workingDir;
        nightmap_url = cfg.nightmap;
        daymap_url = cfg.daymap;
        cloudmap_url = cfg.cloudmap;
        out_file = cfg.outFile;
      };

      restartIfChanged = true;

      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.realwallpaper}/realwallpaper.sh";
        User = "realwallpaper";
      };
    };

    users.extraUsers.realwallpaper = {
      uid = genid "realwallpaper";
      home = cfg.workingDir;
      createHome = true;
    };
  };

in
out

