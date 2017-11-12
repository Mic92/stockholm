with import <stockholm/lib>;
{ config, pkgs, ... }: let
  cfg = config.krebs.per-user;
in {
  options.krebs.per-user = mkOption {
    type = types.attrsOf (types.submodule {
      options = {
        packages = mkOption {
          type = types.listOf types.path;
          default = [];
        };
      };
    });
    default = {};
  };
  config = {
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
}
