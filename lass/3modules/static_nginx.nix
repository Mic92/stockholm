{ config, lib, pkgs, ... }:

with lib;

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
      };
    }));
    default = {};
  };

  user = config.services.nginx.user;
  group = config.services.nginx.group;

  imp = {
    krebs.nginx.servers = flip mapAttrs cfg ( name: { domain, folder, ... }: {
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
    });
  };

in out
