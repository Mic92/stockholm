## generate keys with:
# tinc generate-keys
# ssh-keygen -f ssh.id_ed25519 -t ed25519 -C host

with import <stockholm/lib>;
{ config, ... }: let

  hostDefaults = hostName: host: foldl' recursiveUpdate {} [
    {
      owner = config.krebs.users.makefu;
    }
    # Retiolum defaults
    (let
      pubkey-path = ./retiolum + "/${hostName}.pub";
    in optionalAttrs (pathExists pubkey-path) {
      nets.retiolum = {
        tinc.pubkey = readFile pubkey-path;
        aliases = [
          "${hostName}.r"
        ];
        ip6.addr =
          (krebs.genipv6 "retiolum" "makefu" { inherit hostName; }).address;
      };
    })
    # Retiolum ed25519 keys
    (let
      pubkey-path = ./retiolum + "/${hostName}_ed25519.pub";
    in optionalAttrs (pathExists pubkey-path) {
      nets.retiolum.tinc.pubkey_ed25519 = readFile pubkey-path;
    })
    # Wiregrill defaults
    (let
      pubkey-path = ./wiregrill + "/${hostName}.pub";
    in optionalAttrs (pathExists pubkey-path) {
      nets.wiregrill = {
        aliases = [
          "${hostName}.w"
        ];
        ip6.addr =
          (krebs.genipv6 "wiregrill" "makefu" { inherit hostName; }).address;
        wireguard.pubkey = readFile pubkey-path;
      };
    })
    # SSHD defaults
    (let
      pubkey-path = ./sshd + "/${hostName}.pub";
    in optionalAttrs (pathExists pubkey-path) {
      ssh.pubkey = readFile pubkey-path;
      # We assume that if the sshd pubkey exits then there must be a privkey in
      # the screts store as well
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
    })
    host
  ];

  pub-for = name: builtins.readFile (./ssh + "/${name}.pub");
  w6 = ip: (krebs.genipv6 "wiregrill" "makefu" ip).address;
in {
  hosts = mapAttrs hostDefaults {
    cake = rec {
      cores = 4;
      ci = false;
      nets = {
        retiolum.ip4.addr = "10.243.136.236";
      };
    };
    crapi = rec { # raspi1
      cores = 1;
      ci = false;
      nets = {
        retiolum.ip4.addr = "10.243.136.237";
      };
    };
    firecracker = {
      cores = 4;
      nets = {
        retiolum.ip4.addr = "10.243.12.12";
      };
    };

    studio = rec {
      ci = false;
      cores = 4;
      nets = {
        retiolum.ip4.addr = "10.243.227.163";
      };
    };
    fileleech = rec {
      ci = false;
      cores = 4;
      nets = {
        retiolum.ip4.addr = "10.243.113.98";
      };
    };
    tsp = {
      ci = true;
      cores = 1;
      nets = {
        retiolum.ip4.addr = "10.243.0.212";
      };
    };
    x = {
      ci = true;
      cores = 4;
      syncthing.id = "OA36OF6-JEFCUJQ-OEYVTMH-DPCACQI-3AJRE5G-BFVMOUG-RPYJQE3-4ZCUWA5";
      nets = {
        retiolum.ip4.addr = "10.243.0.91";
        wiregrill = {
          # defaults
        };
      };

    };
    filepimp = rec {
      ci = false;
      cores = 1;
      nets = {
        retiolum.ip4.addr = "10.243.153.102";
      };
    };

    omo = rec {
      ci = true;
      cores = 2;
      syncthing.id = "Y5OTK3S-JOJLAUU-KTBXKUW-M7S5UEQ-MMQPUK2-7CXO5V6-NOUDLKP-PRGAFAK";
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.89";
          aliases = [
            "omo.r"
            "dcpp.omo.r"
            "backup.makefu.r"
            "torrent.omo.r"
            "music.omo.r"
            "music.makefu.r"
          ];
        };
      };
    };
    wbob = rec {
      ci = true;
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.214.15";
          aliases = [
            "wbob.r"
            "hydra.wbob.r"
            "log.wbob.r"
          ];
        };
      };
    };
    latte = rec {
      ci = true;
      extraZones = {
        "krebsco.de" = ''
          latte.euer     IN A      ${nets.internet.ip4.addr}
        '';
      };
      cores = 4;
      nets = rec {
        internet = {
          ip4.addr = "178.254.30.202";
          ip6.addr = "2a00:6800:3:18c::2";
          aliases = [
            "latte.i"
          ];
        };
        #wiregrill = {
        #  via = internet;
        #  ip4.addr = "10.244.245.1";
        #  ip6.addr = w6 "1";
        #  wireguard.port = 51821;
        #  wireguard.subnets = [
        #      (krebs.genipv6 "wiregrill" "makefu" 0).subnetCIDR
        #      "10.244.245.0/24" # required for routing directly to gum via rockit
        #  ];
        #};
        retiolum = {
          via = internet;
          ip4.addr = "10.243.0.214";
          # never connect via gum (he eats your packets!)
          #tinc.weight = 9001;

          aliases = [
            "latte.r"
            "torrent.latte.r"
          ];
        };
      };
    };
    gum = rec {
      ci = true;
      extraZones = {
        "krebsco.de" = ''
          rss.euer          IN A      ${nets.internet.ip4.addr}
          o.euer            IN A      ${nets.internet.ip4.addr}
          bw.euer           IN A      ${nets.internet.ip4.addr}
          bookmark.euer     IN A      ${nets.internet.ip4.addr}
          boot              IN A      ${nets.internet.ip4.addr}
          boot.euer         IN A      ${nets.internet.ip4.addr}
          cache.euer        IN A      ${nets.internet.ip4.addr}
          cache.gum         IN A      ${nets.internet.ip4.addr}
          cgit.euer         IN A      ${nets.internet.ip4.addr}
          dl.euer           IN A      ${nets.internet.ip4.addr}
          dns.euer          IN A      ${nets.internet.ip4.addr}
          dockerhub         IN A      ${nets.internet.ip4.addr}
          euer              IN A      ${nets.internet.ip4.addr}
          euer              IN MX 1   aspmx.l.google.com.
          ghook             IN A      ${nets.internet.ip4.addr}
          git.euer          IN A      ${nets.internet.ip4.addr}
          gold              IN A      ${nets.internet.ip4.addr}
          graph             IN A      ${nets.internet.ip4.addr}
          gum               IN A      ${nets.internet.ip4.addr}
          io                IN NS     gum.krebsco.de.
          iso.euer          IN A      ${nets.internet.ip4.addr}
          feed.euer         IN A      ${nets.internet.ip4.addr}
          board.euer        IN A      ${nets.internet.ip4.addr}
          etherpad.euer     IN A      ${nets.internet.ip4.addr}
          mediengewitter    IN CNAME  over.dose.io.
          mon.euer          IN A      ${nets.internet.ip4.addr}
          netdata.euer      IN A      ${nets.internet.ip4.addr}
          nixos.unstable    IN CNAME  krebscode.github.io.
          photostore        IN A      ${nets.internet.ip4.addr}
          pigstarter        IN CNAME  makefu.github.io.
          share.euer        IN A      ${nets.internet.ip4.addr}
          wg.euer           IN A      ${nets.internet.ip4.addr}
          wiki.euer         IN A      ${nets.internet.ip4.addr}
          wikisearch        IN A      ${nets.internet.ip4.addr}

          meet.euer         IN A      ${nets.internet.ip4.addr}
          work.euer         IN A      ${nets.internet.ip4.addr}
          admin.work.euer   IN A      ${nets.internet.ip4.addr}
          push.work.euer    IN A      ${nets.internet.ip4.addr}
          api.work.euer     IN A      ${nets.internet.ip4.addr}
          maps.work.euer    IN A      ${nets.internet.ip4.addr}
          play.work.euer    IN A      ${nets.internet.ip4.addr}
          ul.work.euer      IN A      ${nets.internet.ip4.addr}
          music.euer        IN A      ${nets.internet.ip4.addr}
        '';
      };
      cores = 8;
      nets = rec {
        internet = {
          ip4.addr = "142.132.189.140";
          ip6.addr = "fe80::9400:1ff:fe24:33f4";
          aliases = [
            "gum.i"
          ];
        };
        wiregrill = {
          via = internet;
          ip4.addr = "10.244.245.1";
          ip6.addr = w6 "1";
          wireguard.port = 51821;
          wireguard.subnets = [
              (krebs.genipv6 "wiregrill" "makefu" 0).subnetCIDR
              "10.244.245.0/24" # required for routing directly to gum via rockit
          ];
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.0.213";
          # never connect via gum (he eats your packets!)
          #tinc.weight = 9001;

          aliases = [
            "gum.r"
            "blog.gum.r"
            "blog.makefu.r"
            "cache.gum.r"
            "cgit.gum.r"
            "dcpp.gum.r"
            "dcpp.nextgum.r"
            "graph.makefu.r"
            "logs.makefu.r"
            "netdata.makefu.r"
            "nextgum.r"
            "o.gum.r"
            "search.makefu.r"
            "stats.makefu.r"
            "torrent.gum.r"
            "tracker.makefu.r"
            "wiki.gum.r"
            "wiki.makefu.r"
            "warrior.gum.r"
            "rss.makefu.r"
            "sick.makefu.r"
            "dl.gum.r"
            "dl.makefu.r"
          ];
        };
      };
    };

    sdev = rec {
      ci = true;
      cores = 1;
      nets = {
        retiolum.ip4.addr = "10.243.83.237";
      };
    };


# non-stockholm

    flap = rec {
      cores = 1;
      extraZones = {
        "krebsco.de" = ''
          flap              IN A      ${nets.internet.ip4.addr}
        '';
      };
      nets = {
        internet = {
          ip4.addr = "162.248.11.162";
          aliases = [
            "flap.i"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.211.172";
        };
      };
    };

    nukular = rec {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.231.219";
        };
      };
    };


    shackdev = rec { # router@shack
      cores = 1;
      nets.wiregrill.ip4.addr = "10.244.245.2";
    };

    rockit = rec { # router@home
      cores = 1;
      nets.wiregrill.ip4.addr = "10.244.245.3";
    };

    senderechner = rec {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.163";
        };
      };
    };
  };
  users = rec {
    makefu = {
      mail = "makefu@x.r";
      pubkey = pub-for "makefu.x";
      pgp.pubkeys.default = builtins.readFile ./pgp/default.asc;
      pgp.pubkeys.brain = builtins.readFile ./pgp/brain.asc;
    };
    makefu-omo = {
      inherit (makefu) mail pgp;
      pubkey = pub-for "makefu.omo";
    };
    makefu-tsp = {
      inherit (makefu) mail pgp;
      pubkey = pub-for "makefu.tsp";
    };
    makefu-vbob = {
      inherit (makefu) mail pgp;
      pubkey = pub-for "makefu.vbob";
    };
    makefu-tempx = {
      inherit (makefu) mail pgp;
      pubkey = pub-for "makefu.tempx";
    };
    makefu-android = {
      inherit (makefu) mail pgp;
      pubkey = pub-for "makefu.android";
    };
    makefu-remote-builder = {
      inherit (makefu) mail pgp;
      pubkey = pub-for "makefu.remote-builder";
    };
    makefu-bob = {
      inherit (makefu) mail pgp;
      pubkey = pub-for "makefu.bob";
    };
  };
}
