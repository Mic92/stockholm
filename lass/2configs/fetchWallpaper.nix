{ config, pkgs, ... }:

let

in {
  krebs.fetchWallpaper = {
    enable = true;
    url = "prism/realwallpaper-krebs-stars-berlin.png";
  };
}

