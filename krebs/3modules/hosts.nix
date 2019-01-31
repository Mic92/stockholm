with import <stockholm/lib>;
{ config, ... }: let
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

    nixpkgs.config.packageOverrides = super: let
      # nameValuePair name value : { "name" : name, "value" : value }

      # addr : str
      # aliase : str
      # hostname : str
      # netname : str

      # addrAliases : nameValuePair addr [alias]

      # hostNetAliases : host -> { ${netname} : [addrAliases] }
      hostNetAliases = host:
        mapAttrs (_: net: filter (x: x.name != null) [
          { name = net.ip4.addr or null; value = net.aliases; }
          { name = net.ip6.addr or null; value = net.aliases; }
        ]) host.nets;

      # netAliases : { ${netname} : [addrAliases] }
      netAliases =
        foldl'
          (result: host:
            foldl'
              # λ netAliases -> [addrAliases] -> netAliases
              (result: { name, value }: result // {
                ${name} = result.${name} or [] ++ value;
              })
              result
              (mapAttrsToList nameValuePair (hostNetAliases host))
          )
          {}
          (attrValues config.krebs.hosts);

      # allAddrAliases : [addrAliases]
      allAddrAliases =
        flatten
          (map
            (host: attrValues (hostNetAliases host))
            (attrValues config.krebs.hosts));

      # writeHosts : str -> [addrAliases] -> package
      writeHosts = name: addrAliases: super.writeText name ''
        ${concatMapStringsSep
            "\n"
            ({ name, value }: "${name} ${toString value}")
            addrAliases}
      '';
    in
      {
        krebs-hosts = writeHosts "krebs-hosts" allAddrAliases;
      }
      //
      genAttrs' (attrNames netAliases) (netname: rec {
        name = "krebs-hosts-${netname}";
        value = writeHosts name netAliases.${netname};
      });
  };

}
