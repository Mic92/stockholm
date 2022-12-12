{ config, ... }: let
  lib = import ../../lib;
in {
  options.krebs.users = lib.mkOption {
    type = with lib.types; attrsOf user;
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
