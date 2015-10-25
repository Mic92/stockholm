{ config, lib, ... }:

with lib;
let
  cfg = config.krebs;

  out = {
    imports = [
      ./bepasty-server.nix
      ./build.nix
      ./current.nix
      ./exim-retiolum.nix
      ./exim-smarthost.nix
      ./github-hosts-sync.nix
      ./git.nix
      ./iptables.nix
      ./nginx.nix
      ./Reaktor.nix
      ./retiolum-bootstrap.nix
      ./realwallpaper.nix
      ./retiolum.nix
      ./tinc_graphs.nix
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
    { krebs = import ./shared { inherit lib; }; }
    { krebs = import ./tv { inherit lib; }; }
    {
      krebs.dns.providers = {
        de.krebsco = "zones";
        gg23 = "hosts";
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
        stripEmptyLines = s: concatStringsSep "\n"
          (remove "\n" (remove "" (splitString "\n" s)));
        all-zones = foldAttrs (sum: current: sum + "\n" +current ) ""
          ([cfg.zone-head-config] ++ combined-hosts);
        combined-hosts = (mapAttrsToList (name: value: value.extraZones)  cfg.hosts );
      in lib.mapAttrs' (name: value: nameValuePair
        ("zones/" + name)
        { text=(stripEmptyLines value); }) all-zones;

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
        "postmaster@krebsco.de" = spam-ml; # RFC 822
        "lass@krebsco.de" = lass;
        "makefu@krebsco.de" = makefu;
        "spam@krebsco.de" = spam-ml;
        "tv@krebsco.de" = tv;
        # XXX These are no internet aliases
        # XXX exim-retiolum hosts should be able to relay to retiolum addresses
        "lass@retiolum" = lass;
        "makefu@retiolum" = makefu;
        "spam@retiolum" = spam-ml;
        "tv@retiolum" = tv;
      });

      services.openssh.hostKeys =
        let inherit (config.krebs.build.host.ssh) privkey; in
        mkIf (privkey != null) (mkForce [privkey]);

      services.openssh.knownHosts =
        # GitHub's IPv4 address range is 192.30.252.0/22
        # Refs https://help.github.com/articles/what-ip-addresses-does-github-use-that-i-should-whitelist/
        # 192.30.252.0/22 = 192.30.252.0-192.30.255.255 (1024 addresses)
        # Because line length is limited by OPENSSH_LINE_MAX (= 8192),
        # we split each /24 into its own entry.
        listToAttrs (map
          (c: {
            name = "github${toString c}";
            value = {
              hostNames = ["github.com"] ++
                map (d: "192.30.${toString c}.${toString d}") (range 0 255);
              publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
            };
          })
          (range 252 255))
        //
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
