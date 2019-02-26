## generate keys with:
# tinc generate-keys
# ssh-keygen -f ssh.id_ed25519 -t ed25519 -C host

with import <stockholm/lib>;
{ config, ... }: let

  hostDefaults = hostName: host: flip recursiveUpdate host ({
    owner = config.krebs.users.makefu;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum.ip6.addr =
      (krebs.genipv6 "retiolum" "makefu" { inherit hostName; }).address;
  });

  pub-for = name: builtins.readFile (./ssh + "/${name}.pub");
  sshd-for = name: builtins.readFile (./sshd + "/${name}.pub");
  tinc-for= name: builtins.readFile (./tinc + "/${name}.pub");

in {
  hosts = mapAttrs hostDefaults {
    cake = rec {
      cores = 4;
      ci = false;
      nets = {
        retiolum = {
          ip4.addr = "10.243.136.236";
          aliases = [
            "cake.r"
          ];
          tinc.pubkey = tinc-for "cake";
        };
      };
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = sshd-for "cake";
    };
    crapi = rec { # raspi1
      cores = 1;
      ci = false;
      nets = {
        retiolum = {
          ip4.addr = "10.243.136.237";
          aliases = [
            "crapi.r"
          ];
          tinc.pubkey = tinc-for "crapi";
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = sshd-for "crapi";
    };
    firecracker = {
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.12.12";
          ip6.addr = "42:0:0:0:0:0:0:12";
          aliases = [
            "firecracker.r"
          ];
          tinc.pubkey = tinc-for "firecracker";
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = sshd-for "firecracker";
    };

    studio = rec {
      ci = false;
      cores = 4;
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = sshd-for "studio";
      nets = {
        retiolum = {
          ip4.addr = "10.243.227.163";
          aliases = [
            "studio.r"
          ];
          tinc.pubkey = tinc-for "studio";
        };
      };
    };
    fileleech = rec {
      ci = false;
      cores = 4;
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = "";
      nets = {
        retiolum = {
          ip4.addr = "10.243.113.98";
          aliases = [
            "fileleech.r"
          ];
          tinc.pubkey = tinc-for "fileleech";
        };
      };
    };
    tsp = {
      ci = true;
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.212";
          aliases = [
            "tsp.r"
          ];
          tinc.pubkey = tinc-for "tsp";
        };
      };
    };
    x = {
      ci = true;
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.91";
          aliases = [
            "x.r"
          ];
          tinc.pubkey = tinc-for "x";
        };
        #wiregrill = {
        #  aliases = [
        #    "x.w"
        #  ];
        #  wireguard.pubkey = "fe5smvKVy5GAn7EV4w4tav6mqIAKhGWQotm7dRuRt1g=";
        #};
      };
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = sshd-for "x";

    };
    filepimp = rec {
      ci = false;
      cores = 1;
      nets = {
        lan = {
          ip4.addr = "192.168.1.12";
          aliases = [
            "filepimp.lan"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.153.102";
          aliases = [
            "filepimp.r"
          ];
          tinc.pubkey = tinc-for "filepimp";
        };
      };
    };

    omo = rec {
      ci = true;
      cores = 2;

      nets = {
        lan = {
          ip4.addr = "192.168.1.11";
          aliases = [
            "omo.lan"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.0.89";
          aliases = [
            "omo.r"
            "dcpp.omo.r"
            "torrent.omo.r"
          ];
          tinc.pubkey = tinc-for "omo";
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = sshd-for "omo";
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
          ];
          tinc.pubkey = tinc-for "wbob";
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = sshd-for "wbob";
    };
    gum = rec {
      ci = true;
      extraZones = {
        "krebsco.de" = ''
          boot              IN A      ${nets.internet.ip4.addr}
          boot.euer         IN A      ${nets.internet.ip4.addr}
          cache.euer        IN A      ${nets.internet.ip4.addr}
          cache.gum         IN A      ${nets.internet.ip4.addr}
          cgit.euer         IN A      ${nets.internet.ip4.addr}
          dl.euer           IN A      ${nets.internet.ip4.addr}
          dockerhub         IN A      ${nets.internet.ip4.addr}
          euer              IN A      ${nets.internet.ip4.addr}
          euer              IN MX 1   aspmx.l.google.com.
          ghook             IN A      ${nets.internet.ip4.addr}
          git.euer          IN A      ${nets.internet.ip4.addr}
          gold              IN A      ${nets.internet.ip4.addr}
          graph             IN A      ${nets.internet.ip4.addr}
          gum               IN A      ${nets.internet.ip4.addr}
          iso.euer          IN A      ${nets.internet.ip4.addr}
          mon.euer          IN A      ${nets.internet.ip4.addr}
          netdata.euer      IN A      ${nets.internet.ip4.addr}
          nixos.unstable    IN CNAME  krebscode.github.io.
          o.euer            IN A      ${nets.internet.ip4.addr}
          photostore        IN A      ${nets.internet.ip4.addr}
          pigstarter        IN A      ${nets.internet.ip4.addr}
          share.euer        IN A      ${nets.internet.ip4.addr}
          wg.euer           IN A      ${nets.internet.ip4.addr}
          wiki.euer         IN A      ${nets.internet.ip4.addr}
          wikisearch        IN A      ${nets.internet.ip4.addr}
          io                IN NS     gum.krebsco.de.
        '';
      };
      cores = 8;
      nets = rec {
        internet = {
          ip4.addr = "144.76.26.247";
          ip6.addr = "2a01:4f8:191:12f6::2";
          aliases = [
            "gum.i"
            "nextgum.i"
          ];
        };
        #wiregrill = {
        #  via = internet;
        #  aliases = [
        #    "gum.w"
        #  ];
        #  wireguard.pubkey = "yAKvxTvcEVdn+MeKsmptZkR3XSEue+wSyLxwcjBYxxo=";
        #};
        retiolum = {
          via = internet;
          ip4.addr = "10.243.0.213";
          aliases = [
            "backup.makefu.r"
            "blog.gum.r"
            "blog.makefu.r"
            "cache.gum.r"
            "cgit.gum.r"
            "dcpp.gum.r"
            "dcpp.nextgum.r"
            "graph.r"
            "gum.r"
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
          ];
          tinc.pubkey = tinc-for "gum";
        };
      };
      ssh.pubkey = sshd-for "gum";
    };

    sdev = rec {
      ci = true;
      cores = 1;
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = sshd-for "sdev";
      nets = {
        retiolum = {
          ip4.addr = "10.243.83.237";
          aliases = [
            "sdev.r"
          ];
          tinc.pubkey = tinc-for "sdev";
        };
      };
    };


# non-stockholm

    flap = rec {
      cores = 1;
      extraZones = {
        "krebsco.de" = ''
          mediengewitter    IN A      ${nets.internet.ip4.addr}
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
          aliases = [
            "flap.r"
          ];
          tinc.pubkey = tinc-for "flap";
        };
      };
    };

    nukular = rec {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.231.219";
          aliases = [
            "nukular.r"
          ];
          tinc.pubkey = tinc-for "nukular";
        };
      };
    };

    filebitch = rec {
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.189.130";
          aliases = [
            "filebitch.r"
          ];
          tinc.pubkey = tinc-for "filebitch";
        };
      };
    };

    senderechner = rec {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.163";
          aliases = [
            "senderechner.r"
          ];
          tinc.pubkey = tinc-for "senderechner";
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
