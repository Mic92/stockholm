{ config, lib, pkgs, ... }:
with import ../../lib/pure.nix { inherit lib; }; {
  options = {
    krebs.dns.providers = mkOption {
      type = types.attrsOf types.str;
    };
    krebs.dns.search-domain = mkOption {
      type = types.nullOr types.hostname;
    };
  };
  config = lib.mkIf config.krebs.enable {
    krebs.dns.providers = {
      "krebsco.de" = "zones";
      shack = "hosts";
      i = "hosts";
      r = "hosts";
      w = "hosts";
    };
    krebs.dns.search-domain = mkDefault "r";
  };
}
