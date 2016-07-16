{ config, ... }:

with config.krebs.lib;

{
  options.krebs.build = {
    # TODO deprecate krebs.build.host
    host = mkOption {
      type = types.host;
    };

    # TODO make krebs.build.profile shell safe
    profile = mkOption {
      type = types.str;
      default = "/nix/var/nix/profiles/system";
    };

    source = mkOption {
      type = types.attrsOf types.source;
      default = {};
    };

    # TODO deprecate krebs.build.user
    user = mkOption {
      type = types.user;
    };
  };
}
