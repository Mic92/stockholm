{ config, pkgs, ... }:

let

in {
  krebs.fetchWallpaper = {
    enable = true;
    unitConfig.ConditionPathExists = "!/var/run/ppp0.pid";
    url = "prism/realwallpaper-krebs.png";
  };
}

