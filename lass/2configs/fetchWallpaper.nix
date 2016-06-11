{ config, pkgs, ... }:

let

in {
  krebs.fetchWallpaper = {
    enable = true;
    url = "prism/wallpaper.png";
  };
}

