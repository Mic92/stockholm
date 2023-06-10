{ config, lib, pkgs, ... }:

with import ../../lib/pure.nix { inherit lib; };

{
  options.krebs.build = {
    # TODO deprecate krebs.build.host
    host = mkOption {
      type = types.host;
    };

    profile = mkOption {
      type = types.absolute-pathname;
      default = "/nix/var/nix/profiles/system";
    };

    # TODO deprecate krebs.build.user
    user = mkOption {
      type = types.user;
    };
  };
}
