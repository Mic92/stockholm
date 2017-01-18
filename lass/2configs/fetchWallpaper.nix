{ config, pkgs, ... }:

let

in {
  krebs.fetchWallpaper = {
    enable = true;
    unitConfig.ConditionPathExists = "!/var/run/ppp0.pid";
    url = "prism/wallpaper.png";
  };
  systemd.services.fetchWallpaper = {
    after = [ "xserver.service" ];
    wantedBy = [ "xserver.service" ];
  };
}

