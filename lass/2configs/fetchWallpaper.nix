{ config, pkgs, ... }:

let

in {
  krebs.fetchWallpaper = {
    enable = true;
    url = "echelon/wallpaper.png";
  };
}

