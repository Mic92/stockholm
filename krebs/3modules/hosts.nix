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
    networking.extraHosts =
      concatStringsSep
        "\n"
        (flatten
          (mapAttrsToList
            (hostname: host:
              mapAttrsToList
                (netname: net: let
                  aliases = longs ++ shorts;
                  longs = filter check net.aliases;
                  shorts = let s = ".${config.krebs.dns.search-domain}"; in
                    map (removeSuffix s) (filter (hasSuffix s) longs);
                in
                  optionals
                    (aliases != [])
                    (map (addr: "${addr} ${toString aliases}") net.addrs))
                (filterAttrs (name: host: host.aliases != []) host.nets))
            config.krebs.hosts));
  };

}
