{ config, ... }:

with import <stockholm/lib>;

{
  options.krebs.build = {
    # TODO deprecate krebs.build.host
    host = mkOption {
      type = types.host;
    };

    profile = mkOption {
      type = types.absolute-path;
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
