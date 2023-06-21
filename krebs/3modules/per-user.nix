{ config, pkgs, lib, ... }:
with lib; let
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
  config = mkIf (cfg != {}) {
    environment = {
      etc =
        mapAttrs'
          (name: per-user: {
            name = "per-user/${name}";
            value.source = pkgs.buildEnv {
              name = "per-user.${name}";
              paths = per-user.packages;
              pathsToLink = [
                "/bin"
              ];
            };
          })
          (filterAttrs (_: per-user: per-user.packages != []) cfg);
      profiles = ["/etc/per-user/$LOGNAME"];
    };
  };
}
