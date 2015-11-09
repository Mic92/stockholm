{ config, lib, ... }:

with lib;

let
  target = config.krebs.build // { user.name = "root"; };

  out = {
    # TODO deprecate krebs.build.host
    options.krebs.build.host = mkOption {
      type = types.host;
    };

    # TODO make krebs.build.profile shell safe
    options.krebs.build.profile = mkOption {
      type = types.str;
      default = "/nix/var/nix/profiles/system";
    };

    # TODO make krebs.build.target.host :: host
    options.krebs.build.target = mkOption {
      type = with types; nullOr str;
      default = null;
    };

    # TODO deprecate krebs.build.user
    options.krebs.build.user = mkOption {
      type = types.user;
    };

    options.krebs.build.source.dir = mkOption {
      type = let
        default-host = config.krebs.current.host;
      in types.attrsOf (types.submodule ({ config, ... }: {
        options = {
          host = mkOption {
            type = types.host;
            default = default-host;
          };
          path = mkOption {
            type = types.str;
          };
          target-path = mkOption {
            type = types.str;
            default = "/root/${config._module.args.name}";
          };
          url = mkOption {
            type = types.str;
            default = "file://${config.host.name}${config.path}";
          };
        };
      }));
      default = {};
    };

    options.krebs.build.source.git = mkOption {
      type = with types; attrsOf (submodule ({ config, ... }: {
        options = {
          url = mkOption {
            type = types.str; # TODO must be shell safe
          };
          rev = mkOption {
            type = types.str;
          };
          target-path = mkOption {
            type = types.str;
            default = "/root/${config._module.args.name}";
          };
        };
      }));
      default = {};
    };
  };

in out
