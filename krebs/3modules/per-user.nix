{ config, lib, pkgs, ... }:

with config.krebs.lib;

let
  cfg = config.krebs.per-user;

  out = {
    options.krebs.per-user = api;
    config = imp;
  };

  api = mkOption {
    type = with types; attrsOf (submodule {
      options = {
        packages = mkOption {
          type = listOf path;
          default = [];
        };
      };
    });
    default = {};
  };

  imp = {
    environment = {
      etc = flip mapAttrs' cfg (name: { packages, ... }: {
        name = "per-user/${name}";
        value.source = pkgs.symlinkJoin {
          name = "per-user.${name}";
          paths = packages;
        };
      });
      profiles = ["/etc/per-user/$LOGNAME"];
    };
  };

in out
