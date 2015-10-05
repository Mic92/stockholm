{ config, lib, ... }:

with import ../4lib { inherit lib; };
let
  cfg = config.krebs;

  out = {
    imports = [
      ./build.nix
      ./exim-retiolum.nix
      ./exim-smarthost.nix
      ./github-hosts-sync.nix
      ./git.nix
      ./iptables.nix
      ./nginx.nix
      ./Reaktor.nix
      ./retiolum.nix
      ./urlwatch.nix
    ];
    options.krebs = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs";

    dns = {
      providers = mkOption {
        # TODO with types; tree dns.label dns.provider, so we can merge.
        # Currently providers can only be merged if aliases occur just once.
        type = with types; attrsOf unspecified;
      };
    };

    hosts = mkOption {
      type = with types; attrsOf host;
    };

    users = mkOption {
      type = with types; attrsOf user;
    };

    # XXX is there a better place to define search-domain?
    # TODO search-domains :: listOf hostname
    search-domain = mkOption {
      type = types.hostname;
      default = "retiolum";
    };
    zone-head-config  = mkOption {
      type = with types; attrsOf str;
      description = ''
        The zone configuration head which is being used to create the
        zone files. The string for each key is pre-pended to the zone file.
        '';
        # TODO: configure the default somewhere else,
        # maybe use krebs.dns.providers
      default = {

        # github.io -> 192.30.252.154
        "krebsco.de" = ''
          $TTL 86400
          @ IN SOA dns19.ovh.net. tech.ovh.net. (2015052000 86400 3600 3600000 86400)
                                IN NS     ns19.ovh.net.
                                IN NS     dns19.ovh.net.
                                IN A      192.30.252.154
                                IN A      192.30.252.153
        '';
        };
    };
  };

  imp = mkMerge [
    { krebs = import ./lass { inherit lib; }; }
    { krebs = import ./makefu { inherit lib; }; }
    { krebs = import ./tv { inherit lib; }; }
    {
      krebs.dns.providers = {
        de.krebsco = "zones";
        internet = "hosts";
        retiolum = "hosts";
      };

      # XXX This overlaps with krebs.retiolum
      networking.extraHosts = concatStringsSep "\n" (flatten (
        mapAttrsToList (hostname: host:
          mapAttrsToList (netname: net:
            let
              aliases = longs ++ shorts;
              providers = dns.split-by-provider net.aliases cfg.dns.providers;
              longs = providers.hosts;
              shorts =
                map (removeSuffix ".${cfg.search-domain}")
                    (filter (hasSuffix ".${cfg.search-domain}")
                            longs);
            in
              map (addr: "${addr} ${toString aliases}") net.addrs
          ) (filterAttrs (name: host: host.aliases != []) host.nets)
        ) cfg.hosts
      ));

      # Implements environment.etc."zones/<zone-name>"
      environment.etc = let
        all-zones = foldAttrs (sum: current: sum + "\n" +current ) ""
          ([cfg.zone-head-config] ++ combined-hosts) ;
        combined-hosts = (mapAttrsToList (name: value: value.extraZones)  cfg.hosts );
      in lib.mapAttrs' (name: value: nameValuePair (("zones/" + name)) ({ text=value; })) all-zones;

      krebs.exim-smarthost.internet-aliases = let
        format = from: to:
          # TODO assert is-retiolum-mail-address to;
          { inherit from;
            to = if typeOf to == "list"
                   then concatMapStringsSep "," (getAttr "mail") to
                   else to.mail; };
      in mapAttrsToList format (with config.krebs.users; let
        spam-ml = [
          lass
          makefu
          tv
        ];
      in {
        "spam@krebsco.de" = spam-ml;
      });

      services.openssh.hostKeys =
        let inherit (config.krebs.build.host.ssh) privkey; in
        mkIf (privkey != null) (mkForce [privkey]);

      services.openssh.knownHosts =
        mapAttrs
          (name: host: {
            hostNames =
              concatLists
                (mapAttrsToList
                  (net-name: net:
                    let
                      aliases = shorts ++ longs;
                      longs = net.aliases;
                      shorts =
                        map (removeSuffix ".${cfg.search-domain}")
                            (filter (hasSuffix ".${cfg.search-domain}")
                                    longs);
                      add-port = a:
                        if net.ssh.port != null
                          then "[${a}]:${toString net.ssh.port}"
                          else a;
                    in
                    aliases ++ map add-port net.addrs)
                  host.nets);

            publicKey = host.ssh.pubkey;
          })
          (filterAttrs (_: host: host.ssh.pubkey != null) cfg.hosts);
    }
  ];

in
out
