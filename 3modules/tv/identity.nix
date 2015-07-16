{ config, lib, ... }:

with lib;
let
  cfg = config.tv.identity;

  out = {
    options.tv.identity = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "tv.identity";

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
          subdomains = [
            "cgit"
          ];
          addr = "10.243.113.222";
          #addr6 = "42:4522:25f8:36bb:8ccb:0150:231a:2af3";
          #internet-addr = "162.219.5.183";
          cores = 2;
        };
        mkdir = {
          #dc = "cac";
          dc = "tv";
          fqdn = "mkdir.retiolum";
          subdomains = [
            "cgit"
          ];
          addr = "10.243.113.223";
          cores = 1;
        };
        nomic = {
          #dc = "gg";
          dc = "tv";
          fqdn = "nomic.retiolum";
          subdomains = [
            "cgit"
          ];
          addr = "10.243.0.110";
          cores = 2;
        };
        rmdir = {
          #dc = "cac";
          dc = "tv";
          fqdn = "rmdir.retiolum";
          subdomains = [
            "cgit"
          ];
          addr = "10.243.113.224";
          #addr = "42:4522:25f8:36bb:8ccb:0150:231a:2af5";
          cores = 1;
        };
        wu = {
          #dc = "gg";
          dc = "tv";
          fqdn = "wu.retiolum";
          subdomains = [
            "cgit"
          ];
          addr = "10.243.13.37";
          cores = 8;
        };
      };
    };
  };

  imp = {
    networking.extraHosts =
      let
        f = name: { addr, fqdn, subdomains, ... }: ''
          ${addr} ${toString (map (s: "${s}.${name} ${s}.${fqdn}") subdomains)}
        '';
      in
      concatStringsSep "\n" (mapAttrsToList f cfg.hosts);
  };

in
out
