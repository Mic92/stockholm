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
        value.source = pkgs.buildEnv {
          name = "per-user.${name}";
          paths = packages;
          pathsToLink = [
            "/bin"
          ];
        };
      });
      profiles = ["/etc/per-user/$LOGNAME"];
    };
  };
}
