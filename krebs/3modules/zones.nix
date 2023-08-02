{ config, pkgs, lib, ... }:
with lib; {

  options.krebs.zone-head-config = mkOption {
    type = lib.types.attrsOf lib.types.str;
    description = ''
      The zone configuration head which is being used to create the
      zone files. The string for each key is pre-pended to the zone file.
    '';
    default = {
      "krebsco.de" = /* bindzone */ ''
        $TTL 60
        @ 3600 IN SOA spam.krebsco.de. spam.krebsco.de. 0 7200 3600 86400 3600
        @ 3600 IN NS ns1
        @ 3600 IN NS ni
      '';
    };
  };

  config = {
    environment.etc =
      mapAttrs'
        (name: pkg: {
          name = "zones/${name}";
          value.source = pkg;
        })
        pkgs.krebs.zones;

    nixpkgs.overlays = [
      # Explicit zones generated from config.krebs.hosts.*.extraZones
      (self: super: let
        stripEmptyLines = s: (concatStringsSep "\n"
          (remove "\n" (remove "" (splitString "\n" s)))) + "\n";
        all-zones = foldAttrs (sum: current: sum + "\n" + current) ""
          ([config.krebs.zone-head-config] ++ combined-hosts);
        combined-hosts =
          mapAttrsToList (name: getAttr "extraZones") config.krebs.hosts;
      in {
        krebs = super.krebs or {} // {
          zones = super.krebs.zones or {} //
            mapAttrs'
              (name: value: {
                name = name;
                value = self.writeText "${name}.zone" (stripEmptyLines value);
              })
              all-zones;
        };
      })

      # Implicit zones generated from config.krebs.hosts.*.nets.*.ip{4,6}.addr
      (self: super: let
        # record : { name : str, type : enum [ "A" "AAAA" ], data : str }

        # toRecord : record.name -> record.type -> record.data -> record
        toRecord = name: type: data:
          { inherit name type data; };

        # toRecords : str -> host -> [record]
        toRecords = netname: host:
          let
            net = host.nets.${netname};
          in
          optionals
            (hasAttr netname host.nets)
            (filter
              (x: x.data != null)
              (concatLists [
                (map
                  (name: toRecord name "A" (net.ip4.addr or null))
                  (concatMap
                    (name: [ "${name}." "4.${name}." ])
                    (net.aliases or [])))
                (map
                  (name: toRecord name "AAAA" (net.ip6.addr or null))
                  (concatMap
                    (name: [ "${name}." "6.${name}." ])
                    (net.aliases or [])))
              ]));

        # formatRecord : record -> str
        formatRecord = { name, type, data }: "${name} IN ${type} ${data}";

        # writeZone : attrs -> package
        writeZone =
          { name ? "${domain}.zone"
          , domain ? substring 0 1 netname
          , nameservers ? [ "ni" ]
          , netname
          , hosts ? config.krebs.hosts
          }:
          self.writeText name /* bindzone */ ''
            $TTL 60
            @ IN SOA ns admin 1 3600 600 86400 60
            @ IN NS ns
            ${concatMapStringsSep "\n"
              (name: /* bindzone */ "ns IN CNAME ${name}")
              nameservers
            }
            ${concatMapStringsSep
                "\n"
                formatRecord
                (concatMap
                  (toRecords netname)
                  (attrValues hosts))
            }
          '';
      in {
        krebs = super.krebs or {} // {
          zones = super.krebs.zones or {} // {
            i = writeZone { netname = "internet"; };
            r = writeZone { netname = "retiolum"; };
            w = writeZone { netname = "wiregrill"; };
          };
        };
      })
    ];
  };

}
