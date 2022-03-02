{ config, lib, ... }:

with import <stockholm/lib>;
let
  cfg = config.krebs;

  out = {
    imports = [
      ./acl.nix
      ./airdcpp.nix
      ./announce-activation.nix
      ./apt-cacher-ng.nix
      ./backup.nix
      ./bepasty-server.nix
      ./bindfs.nix
      ./brockman.nix
      ./build.nix
      ./cachecache.nix
      ./ci.nix
      ./current.nix
      ./dns.nix
      ./ergo.nix
      ./exim-retiolum.nix
      ./exim-smarthost.nix
      ./exim.nix
      ./fetchWallpaper.nix
      ./git.nix
      ./github-hosts-sync.nix
      ./github-known-hosts.nix
      ./go.nix
      ./hidden-ssh.nix
      ./hosts.nix
      ./htgen.nix
      ./iana-etc.nix
      ./iptables.nix
      ./kapacitor.nix
      ./konsens.nix
      ./monit.nix
      ./nixpkgs.nix
      ./on-failure.nix
      ./os-release.nix
      ./per-user.nix
      ./permown.nix
      ./power-action.nix
      ./reaktor2.nix
      ./realwallpaper.nix
      ./repo-sync.nix
      ./retiolum-bootstrap.nix
      ./secret.nix
      ./setuid.nix
      ./shadow.nix
      ./ssl.nix
      ./sync-containers.nix
      ./systemd.nix
      ./tinc.nix
      ./tinc_graphs.nix
      ./upstream
      ./urlwatch.nix
      ./xresources.nix
      ./zones.nix
    ];
    options.krebs = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs";

    users = mkOption {
      type = with types; attrsOf user;
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
                                IN A      185.199.108.153
                                IN A      185.199.109.153
                                IN A      185.199.110.153
                                IN A      185.199.111.153
        '';
      };
    };
  };

  imp = lib.mkMerge [
    { krebs = import ./external { inherit config; }; }
    { krebs = import ./external/kmein.nix { inherit config; }; }
    { krebs = import ./external/mic92.nix { inherit config; }; }
    { krebs = import ./external/palo.nix { inherit config; }; }
    { krebs = import ./jeschli { inherit config; }; }
    { krebs = import ./krebs  { inherit config; }; }
    { krebs = import ./lass   { inherit config; }; }
    { krebs = import ./makefu { inherit config; }; }
    { krebs = import ./tv     { inherit config; }; }
    {
      krebs.dns.providers = {
        "krebsco.de" = "zones";
        shack = "hosts";
        i = "hosts";
        r = "hosts";
        w = "hosts";
      };

      krebs.dns.search-domain = mkDefault "r";

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

      services.openssh.hostKeys =
        let inherit (config.krebs.build.host.ssh) privkey; in
        mkIf (privkey != null) [privkey];

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
                        optionals
                          (cfg.dns.search-domain != null)
                          (map (removeSuffix ".${cfg.dns.search-domain}")
                               (filter (hasSuffix ".${cfg.dns.search-domain}")
                                       longs));
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
                (optionalAttrs (cfg.dns.search-domain != null &&
                                hasAttr cfg.dns.search-domain host.nets) {
                  nets."" = host.nets.${cfg.dns.search-domain} // {
                    aliases = [host.name];
                    addrs = [];
                  };
                }))
              config.krebs.hosts)));
    }
  ];

in out
