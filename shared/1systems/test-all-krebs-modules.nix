{ config, pkgs, lib, ... }:
let
  en = { enable = true;};
in {
  krebs = {
    enable = true;
    build.user = config.krebs.users.shared;
    build.host = config.krebs.hosts.test-all-krebs-modules;
    Reaktor.enable = true;
    apt-cacher-ng.enable = true;
    backup.enable = true;
    bepasty.enable = true;
    buildbot.master.enable = true;
    buildbot.slave = {
      enable = true;
      username = "lol";
      password = "wut";
    };
    exim-retiolum.enable = true;
    exim-smarthost = {
      enable = true;
      system-aliases = [ { from = "dick"; to = "butt"; } ];
    };
    go.enable = true;
    iptables = {
      enable = true;
      tables = {};
    };
    nginx.enable = true;
    realwallpaper.enable = true;
    retiolum.enable = true;
    retiolum-bootstrap.enable = true;
    tinc_graphs.enable = true;
    urlwatch.enable = true;
    fetchWallpaper = {
      enable = true;
      url ="localhost";
    };
  };
  # just get the system running
  boot.loader.grub.devices = ["/dev/sda"];
  fileSystems."/" = {
    device = "/dev/lol";
  };
}
