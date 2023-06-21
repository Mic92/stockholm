{ config, lib, pkgs, ... }:
with lib; let
  check = hostname: any (domain: hasSuffix ".${domain}" hostname) domains;
  domains = attrNames (filterAttrs (_: slib.eq "hosts") config.krebs.dns.providers);
  # we need this import because we have infinite recursion otherwise
  slib = import ../../lib/pure.nix { inherit lib; };
in {

  options = {
    krebs.hosts = mkOption {
      default = {};
      type = types.attrsOf slib.types.host;
    };
  };

  config = lib.mkIf config.krebs.enable {
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
                    optionals
                      (config.krebs.dns.search-domain != null)
                      (map (removeSuffix s)
                           (filter (hasSuffix s)
                                   longs));
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
        mapAttrs (_: net: filter (x: x.name != null && x.value != []) [
          { name = net.ip4.addr or null; value = net.aliases; }
          { name = net.ip4.addr or null; value = (map (alias: "4.${alias}") net.aliases); }
          { name = net.ip6.addr or null; value = net.aliases; }
          { name = net.ip6.addr or null; value = (map (alias: "6.${alias}") net.aliases); }
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

      # writeHosts : str -> [addrAliases] -> package
      writeHosts = name: addrAliases: super.writeText name ''
        ${concatMapStringsSep
            "\n"
            ({ name, value }: "${name} ${toString value}")
            addrAliases}
      '';
    in
      {
        # hosts file for all krebs networks
        krebs-hosts =
          writeHosts "krebs-hosts" (concatLists [
            netAliases.internet
            netAliases.retiolum
            netAliases.wiregrill
          ]);

        # combined hosts file for all networks (even custom ones)
        krebs-hosts_combined =
          writeHosts "krebs-hosts_combined"
            (concatLists (attrValues netAliases));
      }
      //
      slib.genAttrs' (attrNames netAliases) (netname: rec {
        name = "krebs-hosts-${netname}";
        value = writeHosts name netAliases.${netname};
      });
  };

}
