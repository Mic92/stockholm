with import <stockholm/lib>;
{ config, ... }: let
  # TODO dedup functions with ./hosts.nix
  check = hostname: any (domain: hasSuffix ".${domain}" hostname) domains;
  domains = attrNames (filterAttrs (_: eq "hosts") config.krebs.dns.providers);
in {
  nixpkgs.config.packageOverrides = super: {
    retiolum-hosts =
      super.writeText "retiolum-hosts" ''
        ${
          concatStringsSep
            "\n"
            (flatten
              (map
                (host: let
                  net = host.nets.retiolum;
                  aliases = longs;
                  longs = filter check net.aliases;
                in
                  optionals
                    (aliases != [])
                    (map (addr: "${addr} ${toString aliases}") net.addrs))
                (filter (host: hasAttr "retiolum" host.nets)
                        (attrValues config.krebs.hosts))))
        }
      '';
  };
}
