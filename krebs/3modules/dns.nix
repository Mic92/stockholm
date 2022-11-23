with import <stockholm/lib>;
{ config, ... }: {
  options = {
    krebs.dns.providers = mkOption {
      type = types.attrsOf types.str;
    };
    krebs.dns.search-domain = mkOption {
      type = types.nullOr types.hostname;
    };
  };
  config = mkIf config.krebs.enable {
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
