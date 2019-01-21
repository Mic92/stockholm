{ config, lib, ... }:

with import <stockholm/lib>;
let
  cfg = config.krebs;

  out = {
    imports = [
      ./airdcpp.nix
      ./announce-activation.nix
      ./apt-cacher-ng.nix
      ./backup.nix
      ./bepasty-server.nix
      ./buildbot/master.nix
      ./buildbot/slave.nix
      ./build.nix
      ./cachecache.nix
      ./charybdis.nix
      ./ci.nix
      ./current.nix
      ./exim.nix
      ./exim-retiolum.nix
      ./exim-smarthost.nix
      ./fetchWallpaper.nix
      ./github-hosts-sync.nix
      ./github-known-hosts.nix
      ./git.nix
      ./go.nix
      ./hidden-ssh.nix
      ./htgen.nix
      ./iana-etc.nix
      ./iptables.nix
      ./kapacitor.nix
      ./konsens.nix
      ./monit.nix
      ./newsbot-js.nix
      ./nixpkgs.nix
      ./on-failure.nix
      ./os-release.nix
      ./per-user.nix
      ./power-action.nix
      ./Reaktor.nix
      ./realwallpaper.nix
      ./retiolum-bootstrap.nix
      ./rtorrent.nix
      ./secret.nix
      ./setuid.nix
      ./tinc.nix
      ./tinc_graphs.nix
      ./urlwatch.nix
      ./repo-sync.nix
      ./xresources.nix
      ./zones.nix
    ];
    options.krebs = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs";

    dns = {
      providers = mkOption {
        type = with types; attrsOf str;
      };
    };

    hosts = mkOption {
      type = with types; attrsOf host;
      default = {};
    };

    users = mkOption {
      type = with types; attrsOf user;
    };

    # XXX is there a better place to define search-domain?
    # TODO search-domains :: listOf hostname
    search-domain = mkOption {
      type = types.hostname;
      default = "r";
    };

    sitemap = mkOption {
      default = {};
      type = types.attrsOf types.sitemap.entry;
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

  imp = lib.mkMerge [
    { krebs = import ./external { inherit config; }; }
    { krebs = import ./jeschli { inherit config; }; }
    { krebs = import ./krebs  { inherit config; }; }
    { krebs = import ./lass   { inherit config; }; }
    { krebs = import ./makefu { inherit config; }; }
    { krebs = import ./tv     { inherit config; }; }
    {
      krebs.dns.providers = {
        "krebsco.de" = "zones";
        gg23 = "hosts";
        shack = "hosts";
        i = "hosts";
        r = "hosts";
        w = "hosts";
      };

      krebs.users = {
        krebs = {
          home = "/krebs";
          mail = "spam@krebsco.de";
        };
        root = {
          home = "/root";
          pubkey = config.krebs.build.host.ssh.pubkey;
          uid = 0;
        };
      };

      networking.extraHosts = let
        domains = attrNames (filterAttrs (_: eq "hosts") cfg.dns.providers);
        check = hostname: any (domain: hasSuffix ".${domain}" hostname) domains;
      in concatStringsSep "\n" (flatten (
        mapAttrsToList (hostname: host:
          mapAttrsToList (netname: net:
            let
              aliases = longs ++ shorts;
              longs = filter check net.aliases;
              shorts = let s = ".${cfg.search-domain}"; in
                map (removeSuffix s) (filter (hasSuffix s) longs);
            in
              optionals
                (aliases != [])
                (map (addr: "${addr} ${toString aliases}") net.addrs)
          ) (filterAttrs (name: host: host.aliases != []) host.nets)
        ) cfg.hosts
      ));

      # TODO dedup with networking.extraHosts
      nixpkgs.config.packageOverrides = oldpkgs:
        let
          domains = attrNames (filterAttrs (_: eq "hosts") cfg.dns.providers);
          check = hostname: any (domain: hasSuffix ".${domain}" hostname) domains;
        in
          {
            retiolum-hosts = oldpkgs.writeText "retiolum-hosts" ''
              ${concatStringsSep "\n" (flatten (
                map (host:
                    let
                      net = host.nets.retiolum;
                      aliases = longs;
                      longs = filter check net.aliases;
                    in
                      optionals
                        (aliases != [])
                        (map (addr: "${addr} ${toString aliases}") net.addrs)
                ) (filter (host: hasAttr "retiolum" host.nets)
                          (attrValues cfg.hosts))))}
            '';
          };

      services.openssh.hostKeys =
        let inherit (config.krebs.build.host.ssh) privkey; in
        mkIf (privkey != null) (mkForce [privkey]);

      # TODO use imports for merging
      services.openssh.knownHosts =
        (let inherit (config.krebs.build.host.ssh) pubkey; in
          optionalAttrs (pubkey != null) {
            localhost = {
              hostNames = ["localhost" "127.0.0.1" "::1"];
              publicKey = pubkey;
            };
          })
        //
        mapAttrs
          (name: host: {
            hostNames =
              concatLists
                (mapAttrsToList
                  (net-name: net:
                    let
                      longs = net.aliases;
                      shorts =
                        map (removeSuffix ".${cfg.search-domain}")
                            (filter (hasSuffix ".${cfg.search-domain}")
                                    longs);
                      add-port = a:
                        if net.ssh.port != 22
                          then "[${a}]:${toString net.ssh.port}"
                          else a;
                    in
                    map add-port (shorts ++ longs ++ net.addrs))
                  host.nets);

            publicKey = host.ssh.pubkey;
          })
          (filterAttrs (_: host: host.ssh.pubkey != null) cfg.hosts);

      programs.ssh.extraConfig = concatMapStrings
        (net: ''
          Host ${toString (net.aliases ++ net.addrs)}
            Port ${toString net.ssh.port}
        '')
        (filter
          (net: net.ssh.port != 22)
          (concatMap (host: attrValues host.nets)
            (mapAttrsToList
              (_: host: recursiveUpdate host
                (optionalAttrs (hasAttr config.krebs.search-domain host.nets) {
                  nets."" = host.nets.${config.krebs.search-domain} // {
                    aliases = [host.name];
                    addrs = [];
                  };
                }))
              config.krebs.hosts)));
    }
  ];

in out
