{ config, pkgs, lib, ... }:
let
  en = { enable = true;};
in {
  imports = [
    ../.
  ];
  krebs = {
    enable = true;
    build.user = config.krebs.users.shared;
    build.host = config.krebs.hosts.test-all-krebs-modules;
    Reaktor.test = {};
    apt-cacher-ng.enable = true;
    backup.enable = true;
    bepasty.enable = true;
    # FIXME fast-tests / instantiate-test-all-modules fails at wolfbot
    # http://wolf:8010/builders/fast-tests/builds/442
    #buildbot.master.enable = true;
    buildbot.worker = {
      enable = true;
      username = "lol";
      password = "wut";
    };
    # XXX exim-retiolum and exim-smarthost are mutually exclusive
    #exim-retiolum = {
    #  enable = true;
    #  primary_hostname = "test.r";
    #};
    exim-smarthost = {
      enable = true;
      primary_hostname = "test.r";
      system-aliases = [ { from = "dick"; to = "butt"; } ];
    };
    go.enable = true;
    iptables = {
      enable = true;
      tables = {};
    };
    nginx.enable = true;
    realwallpaper.enable = true;
    tinc.retiolum.enable = true;
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
