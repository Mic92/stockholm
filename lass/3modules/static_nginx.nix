{ config, lib, pkgs, ... }:

with config.krebs.lib;

let
  cfg = config.lass.staticPage;

  out = {
    options.lass.staticPage = api;
    config = imp;
  };

  api = mkOption {
    type = with types; attrsOf (submodule ({ config, ... }: {
      options = {
        domain = mkOption {
          type = str;
          default = config._module.args.name;
        };
        folder = mkOption {
          type = str;
          default = "/srv/http/${config.domain}";
        };
        #sslEnable = mkEnableOption "ssl";
        #certificate = mkOption {
        #  type = str;
        #};
        #certificate_key = mkOption {
        #  type = str;
        #};
        #ciphers = mkOption {
        #  type = str;
        #  default = "AES128+EECDH:AES128+EDH";
        #};
        ssl = mkOption {
          type = with types; submodule ({
            options = {
              enable = mkEnableOption "ssl";
              certificate = mkOption {
                type = str;
              };
              certificate_key = mkOption {
                type = str;
              };
            };
          });
          default = {};
        };
      };
    }));
    default = {};
  };

  user = config.services.nginx.user;
  group = config.services.nginx.group;

  external-ip = head config.krebs.build.host.nets.internet.addrs4;

  imp = {
    krebs.nginx.servers = flip mapAttrs cfg ( name: { domain, folder, ssl, ... }: {
      server-names = [
        "${domain}"
        "www.${domain}"
      ];
      locations = [
        (nameValuePair "/" ''
          root ${folder};
        '')
        (nameValuePair "~ /\\." ''
          deny all;
        '')
      ];
      inherit ssl;

    });
  };

in out
