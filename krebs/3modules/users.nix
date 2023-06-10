{ config, lib, pkgs, ... }: let
  slib = import ../../lib/pure.nix { inherit lib; };
in {
  options.krebs.users = lib.mkOption {
    type = lib.types.attrsOf slib.types.user;
  };
  config = lib.mkIf config.krebs.enable {
    krebs.users = {
      krebs = {
        home = "/krebs";
        mail = "spam@krebsco.de";
      };
      root = {
        home = "/root";
        pubkey = config.krebs.build.host.ssh.pubkey;
        uid = 0;
      };
    };
  };
}
