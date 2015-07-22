{ config, ... }:

{
  imports = [ ../../3modules/tv/identity.nix ];
  tv.identity = {
    enable = true;
    hosts = {
      cd = {
        cores = 2;
        dc = "tv"; #dc = "cac";
        nets = {
          internet = {
            addrs4 = ["162.219.7.216"];
            aliases = [
              "cd.internet"
              "cd.viljetic.de"
              "cgit.cd.viljetic.de"
              "cd.krebsco.de"
            ];
          };
          retiolum = {
            addrs4 = ["10.243.113.222"];
            addrs6 = ["42:4522:25f8:36bb:8ccb:0150:231a:2af3"];
            aliases = [
              "cd.retiolum"
              "cgit.cd.retiolum"
            ];
          };
        };
        search = "retiolum";
      };
      mkdir = {
        cores = 1;
        dc = "tv"; #dc = "cac";
        nets = {
          retiolum = {
            addrs4 = ["10.243.113.223"];
            aliases = [
              "mkdir.retiolum"
              "cgit.mkdir.retiolum"
            ];
          };
        };
        search = "retiolum";
      };
      nomic = {
        cores = 2;
        dc = "tv"; #dc = "gg23";
        nets = {
          retiolum = {
            addrs4 = ["10.243.0.110"];
            aliases = [
              "nomic.retiolum"
              "cgit.nomic.retiolum"
            ];
          };
        };
        search = "retiolum";
      };
      rmdir = {
        cores = 1;
        dc = "tv"; #dc = "cac";
        nets = {
          retiolum = {
            addrs4 = ["10.243.113.224"];
            addrs6 = ["42:4522:25f8:36bb:8ccb:0150:231a:2af5"];
            aliases = [
              "rmdir.retiolum"
              "cgit.rmdir.retiolum"
            ];
          };
        };
        search = "retiolum";
      };
      wu = {
        cores = 4;
        # TODO wu is mobile, so dc means "home data center"
        dc = "tv"; #dc = "gg23";
        nets = {
          retiolum = {
            addrs4 = ["10.243.13.37"];
            aliases = [
              "wu.retiolum"
            ];
          };
        };
        search = "retiolum";
      };
    };
  };
}
