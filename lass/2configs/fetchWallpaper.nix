{ config, pkgs, ... }:

let

in {
  krebs.fetchWallpaper = {
    enable = true;
    url = "cloudkrebs/wallpaper.png";
  };
}

