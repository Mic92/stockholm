{ lib, ... }:

with lib;

let

  cfg = config.tv.identity;

  out = {
    options.tv.identity = api;
    #config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    self = mkOption {
      type = types.unspecified;
    };
    hosts = mkOption {
      type = with types; attrsOf unspecified;
      default = {
        cd = {
          #dc = "cac";
          dc = "tv";
          fqdn = "cd.retiolum";
          addr = "10.243.113.222";
          #addr6 = "42:4522:25f8:36bb:8ccb:0150:231a:2af3";
          #internet-addr = "162.219.5.183";
          cores = 2;
        };
        mkdir = {
          #dc = "cac";
          dc = "tv";
          fqdn = "mkdir.retiolum";
          addr = "10.243.113.223";
          cores = 1;
        };
        nomic = {
          #dc = "gg";
          dc = "tv";
          fqdn = "nomic.retiolum";
          addr = "10.243.0.110";
          cores = 2;
        };
        rmdir = {
          #dc = "cac";
          dc = "tv";
          fqdn = "rmdir.retiolum";
          addr = "10.243.113.224";
          #addr = "42:4522:25f8:36bb:8ccb:0150:231a:2af5";
          cores = 1;
        };
        wu = {
          #dc = "gg";
          dc = "tv";
          fqdn = "wu.retiolum";
          addr = "10.243.13.37";
          cores = 8;
        };
      };
    };
  };

  #imp = {
  #};

in
out
