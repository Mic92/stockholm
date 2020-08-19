with import <stockholm/lib>;
{
  options = {
    krebs.dns.providers = mkOption {
      type = types.attrsOf types.str;
    };

    krebs.dns.search-domain = mkOption {
      type = types.nullOr types.hostname;
    };
  };
}
