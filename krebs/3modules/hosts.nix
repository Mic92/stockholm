with import <stockholm/lib>;
{ config, ... }: let
  # TODO dedup functions with ./retiolum-hosts.nix
  check = hostname: any (domain: hasSuffix ".${domain}" hostname) domains;
  domains = attrNames (filterAttrs (_: eq "hosts") config.krebs.dns.providers);
in {

  options = {
    krebs.hosts = mkOption {
      default = {};
      type = types.attrsOf types.host;
    };
  };

  config = {
    networking.hosts =
      filterAttrs
        (_name: value: value != [])
        (zipAttrsWith
          (_: concatLists)
          (concatMap
            (host:
              concatMap
                (net: let
                  aliases = longs ++ shorts;
                  longs = filter check net.aliases;
                  shorts = let s = ".${config.krebs.dns.search-domain}"; in
                    map (removeSuffix s) (filter (hasSuffix s) longs);
                in
                  map (addr: { ${addr} = aliases; }) net.addrs)
                (attrValues host.nets))
            (attrValues config.krebs.hosts)));
  };

}
