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

    options.krebs.build.source-version = mkOption {
      type = types.enum [ 1 2 ];
      default = 1;
    };

    options.krebs.build.source = getAttr "v${toString config.krebs.build.source-version}" {
      v1 = {
        dir = mkOption {
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

        git = mkOption {
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

      v2 = let
        raw = types.either types.str types.path;
        url = types.submodule {
          options = {
            url = mkOption {
              type = types.str;
            };
            rev = mkOption {
              type = types.str;
            };
            dev = mkOption {
              type = types.str;
            };
          };
        };
      in mkOption {
        type = types.attrsOf (types.either types.str url);
        apply = let f = mapAttrs (_: value: {
          string = value;
          path = toString value;
          set = f value;
        }.${typeOf value}); in f;
        default = {};
      };
    };

  };

in out
