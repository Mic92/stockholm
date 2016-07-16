{ config, lib, ... }:

with config.krebs.lib;

let
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

    # TODO deprecate krebs.build.user
    options.krebs.build.user = mkOption {
      type = types.user;
    };

    options.krebs.build.source = mkOption {
      type = types.attrsOf types.source;
      default = {};
    };
  };

in out
