{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.tv.per-user;

  out = {
    options.tv.per-user = api;
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
        value.source = pkgs.symlinkJoin "per-user.${name}" packages;
      });
      profiles = ["/etc/per-user/$LOGNAME"];
    };
  };

in out
