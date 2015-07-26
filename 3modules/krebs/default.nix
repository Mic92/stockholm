{ config, lib, ... }:

with import ../../4lib/krebs { inherit lib; };
let
  cfg = config.krebs;

  out = {
    imports = [
      ./github-hosts-sync.nix
      ./git.nix
      ./nginx.nix
      ./retiolum.nix
      ./urlwatch.nix
    ];
    options.krebs = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs";

    build = mkOption {
      type = types.submodule {
        options = {
          host = mkOption {
            type = types.host;
          };
          user = mkOption {
            type = types.user;
          };
        };
      };
      # Define defaul value, so unset values of the submodule get reported.
      default = {};
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
      default = "";
      example = "retiolum";
    };
  };

  imp = mkMerge [
    { krebs = lass-imp; }
    { krebs = makefu-imp; }
    { krebs = tv-imp; }
    {
      # XXX This overlaps with krebs.retiolum
      networking.extraHosts =
        let
          # TODO move domain name providers to a dedicated module
          # providers : tree label providername
          providers = {
            internet = "hosts";
            retiolum = "hosts";
            de.viljetic = "regfish";
            de.krebsco = "ovh";
          };

          # splitByProvider : [alias] -> listset providername alias
          splitByProvider = foldl (acc: alias: listset-insert (providerOf alias) alias acc) {};

          # providerOf : alias -> providername
          providerOf = alias:
            tree-get (splitString "." alias) providers;
        in
        concatStringsSep "\n" (flatten (
          # TODO deepMap ["hosts" "nets"] (hostname: host: netname: net:
          mapAttrsToList (hostname: host:
            mapAttrsToList (netname: net:
              let
                aliases = toString (unique (longs ++ shorts));
                longs = (splitByProvider net.aliases).hosts;
                shorts = map (removeSuffix ".${cfg.search-domain}") longs;
              in
              map (addr: "${addr} ${aliases}") net.addrs
            ) host.nets
          ) config.krebs.hosts
        ));
    }
  ];

  lass-imp = {
    hosts = addNames {
    };
    users = addNames {
      lass = {
        pubkey = readFile ../../Zpubkeys/lass.ssh.pub;
      };
      uriel = {
        pubkey = readFile ../../Zpubkeys/uriel.ssh.pub;
      };
    };
  };

  makefu-imp = { 
    hosts = addNames {
      pnp = {
        cores = 1;
        dc = "makefu"; #vm on 'omo'
        nets = {
          retiolum = {
            addrs4 = ["10.243.0.210"];
            addrs6 = ["42:f9f1:0000:0000:0000:0000:0000:0001"];
            aliases = [
              "pnp.retiolum"
              "cgit.pnp.retiolum"
            ];
            tinc.pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEAugkgEK4iy2C5+VZHwhjj/q3IOhhazE3TYHuipz37KxHWX8ZbjH+g
              Ewtm79dVysujAOX8ZqV8nD8JgDAvkIZDp8FCIK0/rgckhpTsy1HVlHxa7ECrOS8V
              pGz4xOxgcPFRbv5H2coHtbnfQc4GdA5fcNedQ3BP3T2Tn7n/dbbVs30bOP5V0EMR
              SqZwNmtqaDQxOvjpPg9EoHvAYTevrpbbIst9UzCyvmNli9R+SsiDrzEPgB7zOc4T
              TG12MT+XQr6JUu4jPpzdhb6H/36V6ADCIkBjzWh0iSfWGiFDQFinD+YSWbA1NOTr
              Qtd1I3Ov+He7uc2Z719mb0Og2kCGnCnPIwIDAQAB
              -----END RSA PUBLIC KEY-----
              '';
          };
        };
      };
    };
    users = addNames {
      makefu = {
        pubkey = readFile ../../Zpubkeys/makefu_arch.ssh.pub;
      };
    };
  };

  tv-imp = {
    hosts = addNames {
      cd = {
        cores = 2;
        dc = "tv"; #dc = "cac";
        nets = rec {
          internet = {
            addrs4 = ["162.219.7.216"];
            aliases = [
              "cd.internet"
              "cd.viljetic.de"
              "cgit.cd.viljetic.de"
              "cd.krebsco.de"
            ];
          };
          retiolum = {
            via = internet;
            addrs4 = ["10.243.113.222"];
            addrs6 = ["42:4522:25f8:36bb:8ccb:0150:231a:2af3"];
            aliases = [
              "cd.retiolum"
              "cgit.cd.retiolum"
            ];
            tinc.pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIICCgKCAgEAvmCBVNKT/Su4v9nl/Nm3STPo5QxWPg7xEkzIs3Oh39BS8+r6/7UQ
              rebib7mczb+ebZd+Rg2yFoGrWO8cmM0VcLy5bYRMK7in8XroLEjWecNNM4TRfNR4
              e53+LhcPdkxo0A3/D+yiut+A2Mkqe+4VXDm/JhAiAYkZTn7jUtj00Atrc7CWW1gN
              sP3jIgv4+CGftdSYOB4dm699B7OD9XDLci2kOaFqFl4cjDYUok03G0AduUlRx10v
              CKbKOTIdm8C36A902/3ms+Hyzkruu+VagGIZuPSwqXHJPCu7Ju+jarKQstMmpQi0
              PubweWDL0o/Dfz2qT3DuL4xDecIvGE6kv3m41hHJYiK+2/azTSehyPFbsVbL7w0V
              LgKN3usnZNcpTsBWxRGT7nMFSnX2FLDu7d9OfCuaXYxHVFLZaNrpccOq8NF/7Hbk
              DDW81W7CvLyJDlp0WLnAawSOGTUTPoYv/2wAapJ89i8QGCueGvEc6o2EcnBVMFEW
              ejWTQzyD816f4RsplnrRqLVlIMbr9Q/n5TvlgjjhX7IMEfMy4+7qLGRQkNbFzgwK
              jxNG2fFSCjOEQitm0gAtx7QRIyvYr6c7/xiHz4AwxYzBmvQsL/OK57NO4+Krwgj5
              Vk8TQ2jGO7J4bB38zaxK+Lrtfl8i1AK1171JqFMhOc34JSJ7T4LWDMECAwEAAQ==
              -----END RSA PUBLIC KEY-----
            '';
          };
        };
      };
      mkdir = {
        cores = 1;
        dc = "tv"; #dc = "cac";
        nets = rec {
          internet = {
            addrs4 = ["162.248.167.241"];
            aliases = [
              "mkdir.internet"
            ];
          };
          retiolum = {
            via = internet;
            addrs4 = ["10.243.113.223"];
            addrs6 = ["42:4522:25f8:36bb:8ccb:0150:231a:2af4"];
            aliases = [
              "mkdir.retiolum"
              "cgit.mkdir.retiolum"
            ];
            tinc.pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEAuyfM+3od75zOYXqnqRMAt+yp/4z/vC3vSWdjUvEmCuM23c5BOBw+
              dKqbWoSPTzOuaQ0szdL7a6YxT+poSUXd/i3pPz59KgCl192rd1pZoJKgvoluITev
              voYSP9rFQOUrustfDb9qKW/ZY95cwdCvypo7Vf4ghxwDCnlmyCGz7qXTJMLydNKF
              2PH9KiY4suv15sCg/zisu+q0ZYQXUc1TcgpoIYBOftDunOJoNdbti+XjwWdjGmJZ
              Bn4GelsrrpwJFvfDmouHUe8GsD7nTgbZFtiJbKfCEiK16N0Q0d0ZFHhAV2nPjsk2
              3JhG4n9vxATBkO82f7RLrcrhkx9cbLfN3wIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
          };
        };
      };
      nomic = {
        cores = 2;
        dc = "tv"; #dc = "gg23";
        nets = rec {
          retiolum = {
            addrs4 = ["10.243.0.110"];
            addrs6 = ["42:02d5:733f:d6da:c0f5:2bb7:2b18:09ec"];
            aliases = [
              "nomic.retiolum"
              "cgit.nomic.retiolum"
            ];
            tinc.pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEAwb8Yk/YRc17g2J9n960p6j4W/l559OPyuMPdGJ4DmCm3WNQtxoa+
              qTFUiDiI85BcmfqnSeddLG8zTC2XnSlIvCRMJ9oKzppFM4PX4OTAaJZVE5WyCQhw
              Kd4tHVdoQgJW5yFepmT9IUmHqkxXJ0R2W93l2eSZNOcnFvFn0ooiAlRi4zAiHClu
              5Mz80Sc2rvez+n9wtC2D06aYjP23pHYld2xighHR9SUqX1dFzgSXNSoWWCcgNp2a
              OKcM8LzxLV7MTMZFOJCJndZ77e4LsUvxhQFP6nyKZWg30PC0zufZsuN5o2xsWSlA
              Wi9sMB1AUR6mZrxgcgTFpUjbjbLQf+36CwIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
          };
        };
        secure = true;
      };
      rmdir = {
        cores = 1;
        dc = "tv"; #dc = "cac";
        nets = rec {
          internet = {
            addrs4 = ["167.88.44.94"];
            aliases = [
              "rmdir.internet"
            ];
          };
          retiolum = {
            via = internet;
            addrs4 = ["10.243.113.224"];
            addrs6 = ["42:4522:25f8:36bb:8ccb:0150:231a:2af5"];
            aliases = [
              "rmdir.retiolum"
              "cgit.rmdir.retiolum"
            ];
            tinc.pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEA+twy4obSbJdmZLfBoe9YYeyoDnXkO/WPa2D6Eh6jXrWk5fbhBjRf
              i3EAQfLiXXFJX3E8V8YvJyazXklI19jJtCLDiu/F5kgJJfyAkWHH+a/hcg7qllDM
              Xx2CvS/nCbs+p48/VLO6zLC7b1oHu3K/ob5M5bwPK6j9NEDIL5qYiM5PQzV6zryz
              hS9E/+l8Z+UUpYcfS3bRovXJAerB4txc/gD3Xmptq1zk53yn1kJFYfVlwyyz+NEF
              59JZj2PDrvWoG0kx/QjiNurs6XfdnyHe/gP3rmSTrihKFVuA3cZM62sDR4FcaeWH
              SnKSp02pqjBOjC/dOK97nXpKLJgNH046owIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
          };
        };
      };
      wu = {
        cores = 4;
        # TODO wu is mobile, so dc means "home data center"
        dc = "tv"; #dc = "gg23";
        nets = {
          retiolum = {
            addrs4 = ["10.243.13.37"];
            addrs6 = ["42:0:0:0:0:0:0:1337"];
            aliases = [
              "wu.retiolum"
            ];
            tinc.pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEArDvU0cuBsVqTjCX2TlWL4XHSy4qSjUhjrDvUPZSKTVN7x6OENCUn
              M27g9H7j4/Jw/8IHoJLiKnXHavOoc9UJM+P9Fla/4TTVADr69UDSnLgH+wGiHcEg
              GxPkb2jt0Z8zcpD6Fusj1ATs3sssaLHTHvg1D0LylEWA3cI4WPP13v23PkyUENQT
              KpSWfR+obqDl38Q7LuFi6dH9ruyvqK+4syddrBwjPXrcNxcGL9QbDn7+foRNiWw4
              4CE5z25oGG2iWMShI7fe3ji/fMUAl7DSOOrHVVG9eMtpzy+uI8veOHrdTax4oKik
              AFGCrMIov3F0GIeu3nDlrTIZPZDTodbFKQIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
          };
        };
        secure = true;
      };
    };
    users = addNames {
      mv = {
        mail = "mv@cd.retiolum";
        pubkey = readFile ../../Zpubkeys/mv_vod.ssh.pub;
      };
      tv = {
        mail = "tv@wu.retiolum";
        pubkey = readFile ../../Zpubkeys/tv_wu.ssh.pub;
      };
    };
  };

in
out
