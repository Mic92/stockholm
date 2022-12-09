with import ../../lib;
{ config, ... }: let

  evalHost = hostName: hostConfig: evalSubmodule types.host [
    hostConfig
    {
      name = hostName;
      owner = config.krebs.users.tv;
    }
    (optionalAttrs (hasAttrByPath ["nets" "retiolum"] hostConfig) {
      nets.retiolum = {
        ip6.addr =
          (krebs.genipv6 "retiolum" "tv" { inherit hostName; }).address;
      };
    })
    (let
      pubkey-path = ./wiregrill + "/${hostName}.pub";
    in optionalAttrs (pathExists pubkey-path) {
      nets.wiregrill = {
        aliases = [
          "${hostName}.w"
        ];
        ip6.addr =
          (krebs.genipv6 "wiregrill" "tv" { inherit hostName; }).address;
        wireguard.pubkey = readFile pubkey-path;
      };
    })
    (host: mkIf (host.config.ssh.pubkey != null) {
      ssh.privkey = mapAttrs (const mkDefault) {
        path = config.krebs.secret.file "ssh.id_${host.config.ssh.privkey.type}";
        type = head (toList (match "ssh-([^ ]+) .*" host.config.ssh.pubkey));
      };
    })
  ];

in {
  dns.providers = {
    "viljetic.de" = "regfish";
  };
  hosts = mapAttrs evalHost {
    alnus = {
      ci = true;
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.21.1";
          aliases = [
            "alnus.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAyDGucukxY1xFSkqDaicpiCXZe3NX1Max7N+E9PKXO2yE0EFoGdUP
            /4hZFO9IbteDwlsTd/RQIhhUWF818TLWzwasUxgmqBFN4d23IIDLHJxgRZ8cPzAs
            gmBWwnVWRetDETc6HZK6m2rLU6PG53rRLvheZHW/B9nSfUp7n+puehJdGLnBQ8W+
            q5d/yUmN8hqS6h62yfAZEJSr7Gh/AW6Irmf3gjKRJlRmD2z28hR5tFH+Q/ulxJXQ
            rNVzusASjRBO9VYOSWnNWI3Zl9vaUtbtEnvyl3PaV9N3gcHzB2HHlyDIotjqXvxU
            cPLMN0lWOZeDae/9SDT62l/YuETYQo6TxwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "Td6pRkmSzSGVJll26rULdr6W4U87xsHZ/87NEaglW3K";
        };
      };
      ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDP9JS2Nyjx4Pn+/4MrFi1EvBBYVKkGm2Q4lhgaAiSuiGLol53OSsL2KIo01mbcSSBWow9QpQpn8KDoRnT2aMLDrdTFqL20ztDLOXmtrSsz3flgCjmW4f6uOaoZF0RNjAybd1coqwSJ7EINugwoqOsg1zzN2qeIGKYFvqFIKibYFAnQ8hcksmkvPdIO5O8CbdIiP9sZSrSDp0ZyLK2T0PML2jensVZOeqSPulQDFqLsbmavpVLkpDjdzzPRwbZWNB4++YeipbYNOkX4GR1EB4wMZ93IbBV7kpJtib2Zb2AnUf7UW37hxWBjILdstj9ClwNOQggn8kD9ub7YxBzH1dz0Xd8a0mPOAWIDJz9MypXgFRc3vdvPB/W1I4Se0CLbgOkORun9CkgijKr9oEY8JNt8HFd6viZcAaQxOyIm6PNHZTnHfdSc7bIBS2n3e3IZBv0fTd77knGLXg402aTuu2bm/kxsKivxsILXIaGbeXe4ceN3Fynr3FzSM2bUkzHb0mAHu1BQ9YaX0xzCwjVueA5nzGls7ODSFkXsiBfg2FvMN/sTLFca6tnwyqcnD6nujoiS5+BxjDWPgnZYqCaW3B/IkpTsRMsX6QrfhOFcsP8qlJ2Cp82orWoDK/D0vZ9pdzAc6PFGga0RofuJKY2yiq+SRZ7/e9E6VncIVCYZ1OfN0Q==";
    };
    au = {
      ci = true;
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.13.39";
          aliases = [
            "au.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEApD+HJS5gANbZScCMLxgZZgHZUsQUDlyWTLNdANfo0gXQdsYRVE/z
            9zMG/VE9xwy0OC9JM73YaEymXdmWa3kGXP2jjQnOZyJTFMNFHc8dkl+RBnWv8eZm
            PzFN84ZjnYXyOpXJFajR8eelzqlFvD+2WKsXAD5xaW5EmCBTMIjB/zSuLBpqnIHb
            PqQA1XUye69dQRjjcPn1mtYQPS78H8ClJjnhS76owFzyzNZjri1tr2xi2oevnVJG
            cnYNggZHz3Kg3btJQ3VtDKGLJTzHvvMcn2JfPrePR2+KK0/KbMitpYAS687Ikb83
            jjB+eZgXq5g81vc1116bA5yqcT2UNdOPWwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "bfDtJbxusBdosE6dMED32Yc6ZeYI3RFyXryQr7heZpO";
        };
      };
      secure = true;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBsqDuhGJpjpqNv4QmjoOhcODObrPyY3GHLvtVkgXV0g root@au";
    };
    bu = {
      ci = true;
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.13.36";
          aliases = [
            "bu.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAxjAvT1sfHPWExhWRoXG+NJbYUmf5q4yfpfBRvb232LC9sLn4Z2wb
            hxKreR5/j9a/2hRIlCz4IwKftl5vroG9Vy4e7zZIz6QvN4TqED8dUjJ1ubhtj47l
            jjHW4cHLUWsaqqu6TAuPH26qPSxm9VrD6rZIX9RmQ1bWIaonVB3Q+XnDfPlISw6M
            gbQXz4tOsOnC+y/6C3VPUo0nqC+PuA/kyRq/ivVutKd0dTSY8LmCDNla6AEVD5dG
            sIqPWX5h8fjqU7G3oOMvMsBrCkvRRB0F0dQzGo8EXwCDJxa+xOuk5n1GYJ2lqeM/
            st7KIxmLvO5AE7cUxdLlDj4EzVLSDoAqOwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "/MXEuv96HlrpHBto8KP2S6Ztiahhi3H7AevmbYS+xqE";
        };
      };
      secure = true;
      ssh.privkey.path = config.krebs.secret.file "ssh.id_rsa";
      ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC1Y13PvTn+9VjQbgy2ZmpAEFXyYaroYP/5nK9o7B8cidf01Sh39184mG8KN8VuEzCj7b37KnLH8qUDcsukvkxOVSoVHmXH+/Pgbmsxp4c9sxLQLHBfCazhT0S3Zs+BkR6LNQ8GOCS1qsgy05L6fMXoQgds3Zx/X4ZYjLnYVnJo8k+6aP4pU/rB6GFzGG9UrLDvSvk/PoswpEr7S6uFa4bF8JWD5VPkQTPTNwm1LWH4va+ABcw9KOgL2tsAk/jJlkLD4qgXowqgbwcpfe+QCukJb7uIQjRtOgxSAhHqT1nxjS6gROhGt0ojuwALaZaFPr9YtGlqxPhUzAXWKvvbVcr6kkR17HrtXZeLdFqwrUPlkIDFV6yLbYzQGKPFwxtpoJaH/irv6cgeXnHaa9XQJk+XJ5pE0X9uNljGr3B8LMKymdlvvBiWOOLpYsHg5aVOR+K7HvydLSuaah8hpCLjjVyIYIl/pIDL4F/FUSxcFBB4fgdXB77LXm5UizmI7+dqZaOQSm8qXbLZ8P+13ele2JyV1pmvJbLFlhCksDMOXx9jvSJQ6DOjPd+2vtABWh9XGo2Fiy+ekB9LTzlW+xON4FRZDoTPrPmhg40v+s7lySHx3miwCIJfNfLJpf0dxm3pQYWZPIra1RA9hbgstXBJ3+2VA5JEuVRt0SEygN5Kgk1Y5w== root@bu";
    };
    hu = {
      nets = {
        retiolum = {
          ip4.addr = "10.243.13.41";
          aliases = [
            "hu.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAwj5T9Rejp8zGVrHjqA+OeMvcVpax4VazssnRPSUznUEOdVEeSJL5
            8gDBJPtIfxF8iunXr5K7CW036tKvYaGMDwYMOPJZXhFCmU2yUF2g4BcqEhuDdIfO
            +D2Pfr4lc9xO90SKOgwJ53qhf5yqeU/WQ3dpCF/n8k4SUmdafTsvh00UrxYpHuTU
            C22BRXIKR4r/sCJUitWQSWNdSQUxh3lu7sUPr+6sZyJov+eu8oBVlPgYOv6u9nZe
            YhrbCPDKMGPfnQTAtWfHIxNt70Ec5AG6ddQzLeVcM2gP5qi957Fert+C2RNtbz5s
            Brbw1bqZ3P+CGzvxVJZtirvR2f3HkidGPQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "PV8Dz9ni2cPXyJGiG5oU0XWdJkUPgrMzDuzHj7kpMzO";
        };
      };
      secure = true;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO+Rrf9tvuusYlnSZwUiHS4O+AhrpVZ/6n7peSRKojTc root@hu";
    };
    mu = {
      ci = true;
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.20.1";
          aliases = [
            "mu.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEApXErmPSn2CO4V25lqxanCGCFgxEAjdzFUiTCCu0IvELEuCc3PqVA
            g4ecf8gGwPCbzMW/1txjlgbsQcm87U5enaCwzSv/pa7P9/memV74OhqEVOypFlDE
            XeZczqQfNbjoLYl4cKZpTsSZmOgASXaMDrH2N37f50q35C0MQw0HRzaQM5VLrzb4
            o87MClS+yPqpvp34QjW+1lqnOKvMkr6mDrmtcAjCOs9Ma16txyfjGVFi8KmYqIs1
            QEJmyC9Uocz5zuoSLUghgVRn9yl4+MEw6++akFDwKt/eMkcSq0GPB+3Rz/WLDiBs
            FK6BsssQWdwiEWpv6xIl1Fi+s7F0riq2cwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "cEf/Kq/2Fo70yoIcVmhIp4it9eA7L3GdkgrVE9AWU6C";
        };
      };
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1vJsAddvxMA84u9iJEOrIkKn7pQiemMbfW5cfK1d7g root@mu";
    };
    ni = {
      extraZones = {
        "krebsco.de" = ''
          ni          60 IN A ${config.krebs.hosts.ni.nets.internet.ip4.addr}
          ni          60 IN AAAA ${config.krebs.hosts.ni.nets.internet.ip6.addr}
          cgit        60 IN A ${config.krebs.hosts.ni.nets.internet.ip4.addr}
          cgit        60 IN AAAA ${config.krebs.hosts.ni.nets.internet.ip6.addr}
          cgit.ni     60 IN A ${config.krebs.hosts.ni.nets.internet.ip4.addr}
          cgit.ni     60 IN AAAA ${config.krebs.hosts.ni.nets.internet.ip6.addr}
          search.ni   60 IN A ${config.krebs.hosts.ni.nets.internet.ip4.addr}
          search.ni   60 IN AAAA ${config.krebs.hosts.ni.nets.internet.ip6.addr}
          krebsco.de. 60 IN MX 5 ni
          krebsco.de. 60 IN TXT "v=spf1 mx -all"
          tv          300 IN NS ni
        '';
      };
      nets = {
        internet = {
          ip4 = rec {
            addr = "188.68.36.196";
            prefix = "${addr}/32";
          };
          ip6 = rec {
            addr = "2a03:4000:13:4c::1";
            prefix = "${addr}/64";
          };
          aliases = [
            "ni.i"
            "cgit.ni.i"
          ];
          ssh.port = 11423;
        };
        retiolum = {
          via = config.krebs.hosts.ni.nets.internet;
          ip4.addr = "10.243.113.223";
          aliases = [
            "ni.r"
            "cgit.ni.r"
            "search.ni.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA7NHuW8eLVhpBfL70WwcSGVmv4dijKLJs5cH/BmqK8zN2lpiLKt12
            bhaE1YEhGoGma7Kef1Fa0V9xUkJy6C1+sVlfWp/LeY8VRSX5E3u36TEl6kl/4zu6
            Ea/44BoGUSOC9ImxVEX51czA10PFjUSrGFyK0oaRlKNsTwwpNiBOY7/6i74bhn59
            OIsySRUBd2QPjYhJkiuc7gltVfwt6wteZh8R4w2rluVGYLQPsmN/XEWgJbhzI4im
            W+3/bdewHVF1soZWtdocPLeXTn5HETX5g8p2V3bwYL37oIwkCcYxOeQtT7W+lNJ2
            NvIiVh4Phojl4dBUgUQGT0NApMnsaG/4LJpSC4AGiqbsznBdSPhepob7zJggPnWY
            nfAs+YrUUZp1wovhSgWfYTRglRuyYvWkoGbq411H1efawyZ0gcMr+HQlSn2keQOv
            lbcvdgOAxQiEcPVixPq3mTeKaSxWyIJGFceuqtnILGifRNvViX0uo9g5rLQ41PrJ
            9F3azz3gD2Uh73j5pvLU72cge7p1a7epPYWTJYf8oc5JcI3nYTKpSqH8IYaWUjv9
            q0NwOYFDhYtUcTwdbUNl/tUWKyBcovIe7f40723pHSijiPV2WDZC2M/mOc3dvWKF
            Mf00uin+7uMuKtnG6+1z5nKb/AWrqN1RZu0rnG/IkZPKwa19HYsYcOkCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "nDuK96NlNhcxzlX7G30w/706RxItb+FhkFkz/VhUgCE";
        };
        wiregrill = {
          via = config.krebs.hosts.ni.nets.internet;
          ip4.addr = "10.244.3.1";
          wireguard.subnets = [
            (krebs.genipv6 "wiregrill" "tv" 0).subnetCIDR
          ];
        };
      };
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILGDdcKwFm6udU0/x6XGGb87k9py0VlrxF54HeYu9Izb";
    };
    nomic = {
      ci = true;
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.110";
          aliases = [
            "nomic.r"
            "cgit.nomic.r"
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
          tinc.pubkey_ed25519 = "sBevGkYkcNKd39yf/Mp0whnsWIJfTGxSU1lbqN305nP";
        };
      };
      secure = true;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMIHmwXHV7E9UGuk4voVCADjlLkyygqNw054jvrsPn5t root@nomic";
    };
    wu = {
      ci = true;
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.13.37";
          aliases = [
            "wu.r"
            "cgit.wu.r"
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
          tinc.pubkey_ed25519 = "urVOEGxTkBedkpszPH0XRCRMk+Fc2U9IneYMFDqGoIB";
        };
      };
      secure = true;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcJvu8JDVzObLUtlAQg9qVugthKSfitwCljuJ5liyHa";
    };
    querel = {
      ci = true;
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.22.22";
          aliases = [
            "querel.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEArv9eB8acpUhJwRaLY9kGeM7DEPvInVvoduEbec10p4Y2PFx2MjSz
            2OhyxFRkONC4EMV9oVTKD+NRtpbRGZGLYD8ZPB622SvccgB0XnL6ZZfie1feSgrn
            bPyVnX8EnEgtx9IQckHyaxWgtyrluJnY2CbLkCYgD+50KFT12rdHyAa3+QoYU65x
            ACQo28i9xIpsl6dm7iWBb+ecHc7fST35OqWywtVxSpHPe1nvwaYm1p3rqqtkCGVh
            iXE5ruAscri7Dskc5dGR1p7LquhBaebuylH6sfRKA6kre05+/IkXi+JLeAmAtJ+W
            xezYlecEvxhguql9ZmSYAYkR4KknZb56KtvCnm29o0evvEpsaYcbtgq1D0JhoGyk
            4DixS5e+5dg470icVKxPfz1AzejxrTUTtMlI28qjAIx1FcmCBGM+T6yHs/MhNGbf
            aqUmN+FwtsJ2QWFYqu9zjxxyAfrAw+gqHm0LnsKK1ttwF/2fYCTRLowY+ItB3axs
            UVq7DQxyunyYalKGX2RSJ5BHczREHrfgX43HCSlcAuMuow9jHLOjzul0A49rSZ9E
            vOPqbjrki0KEEQj0HN3Ax4UVqZ6mPWaTQzuup+bPQ/2Sjkx6COzMSAPmKo4l6DkA
            J++ZonpnOCUkwCeCU6qJgMuHeXn0uh117Ypj/3J9eKYMO/RTSs3x8l0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPFM2GdL9yOjSBmYBE07ClywNOADc/zxqXwZuWd7Mael root@querel.r";
    };
    xu = {
      binary-cache = {
        pubkey = "xu-1:pYRENvaxZqGeImwLA9qHmRwHV4jfKaYx4u1VcZ31x0s=";
      };
      ci = true;
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.13.38";
          aliases = [
            "xu.r"
            "cgit.xu.r"
            "krebs.xu.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAl3l7IWbfbkVgaJFM3s9g2UCh2rmqoTba16Of7NNWMj05L/hIkUsQ
            uc43/QzidWh/4gEaq5MQ7JpLyzVBQYRJkNlPRF/Z07KdLBskAZCjDYdYue9BrziX
            8s2Irs2+FNbCK2LqtrPhbcXQJvixsk6vjl2OBpWTDUcDEsk+D1YQilxdtyUzCUkw
            mmRo/mzNsLZsYlSgZ6El/ZLkRdtexAzGxJ0DrukpDR0uqXXkp7jUaxRCZ+Cwanvj
            4I1Hu5aHzWB7KJ1SIvpX3a4f+mun1gh3TPqWP5PUqJok1PSuScz6P2UGaLZZyH63
            4o+9nGJPuzb9bpMVRaVGtKXd39jwY7mbqwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "xYgYM9rXS73RFKUHF3ekQWhcWzuBLOPYG2bimhpH2pM";
        };
      };
      secure = true;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPnjfceKuHNQu7S4eYFN1FqgzMqiL7haNZMh2ZLhvuhK root@xu";
    };
    zu = {
      ci = true;
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.13.40";
          aliases = [
            "zu.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAti6y+Qkz80oay6H2+ANROWdH4aJS54ST8VhFxRB3WdnlDFG/9t6d
            idU87uxW5Xmfm6nvpO0OPhG4E3+UI7KtWP71nnducpLV6gfob4f2xNGVG435CJ6u
            BgorbneUbJEfr4Bb0xd46X2BtLqi5/vUY3M5KMGE2sMdyL2/7oujEI8zQJCse95a
            OhDZdF2bCDEixCHahNprkQrD8t1lNYoLR2qtDZ5psIh5vgdp0WOOMGvUkCDkNjWj
            /NKaRXPhUVRDLRFEzMZhtFtSHzaofzrhGFoU1rGZwc/XopqpiFi0D7L++TiNqKAk
            b9cXwDAI50f8dJagPYtIupjN5bmo+QhXcQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDNjHxyUC7afNGSwfwBfQizmDnHTNLWDRHE8SY9W4oiw2lPhCFGTN8Jz84CKtnABbZhbNY1E8T58emF2h45WzDg/OGi8DPAk4VsXSkIhyvAto+nkTy2L4atjqfvXDvqxTDC9sui+t8p5OqOK+sghe4kiy+Vx1jhnjSnkQsx9Kocu24BYTkNqYxG7uwOz6t262XYNwMn13Y2K/yygDR3Uw3wTnEjpaYnObRxxJS3iTECDzgixiQ6ewXwYNggpzO/+EfW1BTz5vmuEVf4GbQ9iEc7IsVXHhR+N0boCscvSgae9KW9MBun0A2veRFXNkkfBEMfzelz+S63oeVfelkBq6N5aLsHYYGC4VQjimScelHYVwxR7O4fV+NttJaFF7H06FJeFzPt3NYZeoPKealD5y2Muh1UnewpmkMgza9hQ9EmI4/G1fMowqeMq0U6Hu0QMDUAagyalizN97AfsllY2cs0qLNg7+zHMPwc5RgLzs73oPUsF3umz0O42I5p5733vveUlWi5IZeI8CA1ZKdpwyMXXNhIOHs8u+yGsOLfSy3RgjVKp2GjN4lfnFd0LI+p7iEsEWDRkIAvGCOFepsebyVpBjGP+Kqs10bPGpk5dMcyn9iBJejoz9ka+H9+JAG04LnXwt6Rf1CRV3VRCRX1ayZEjRv9czV7U9ZpuFQcIlVRJQ== root@zu";
    };
    umz = {
      nets.wiregrill.ip4.addr = "10.244.3.101";
    };
  };
  sitemap = {
    "http://cgit.krebsco.de" = {
      desc = "Git repositories";
    };
    "http://krebs.xu.r" = {
      desc = "krebs-pages mirror";
    };
  };
  users = {
    dv = {
      mail = "dv@alnus.r";
    };
    itak = {
    };
    mv-ni = {
      mail = "mv@ni.r";
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGer9e2+Lew7vnisgBbsFNECEIkpNJgEaqQqgb9inWkQ mv@vod";
    };
    tv = {
      mail = "tv@nomic.r";
      pgp.pubkeys.default = readFile ./pgp/CBF89B0B.asc;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEAQDFR//RnCvEZAt0F6ExDsatKZ/DDdifanuSL360mqOhaFieKI34RoOwfQT9T+Ga52Vh5V2La6esvlph686EdgzeKLvDoxEwFM9ZYFBcMrNzu4bMTlgE7YUYw5JiORyXNfznBGnme6qpuvx9ibYhUyiZo99kM8ys5YrUHrP2JXQJMezDFZHxT4GFMOuSdh/1daGoKKD6hYL/jEHX8CI4E3BSmKK6ygYr1fVX0K0Tv77lIi5mLXucjR7CytWYWYnhM6DC3Hxpv2zRkPgf3k0x/Y1hrw3V/r0Me5h90pd2C8pFaWA2ZoUT/fmyVqvx1tZPYToU/O2dMItY0zgx2kR0yD+6g7Aahz3R+KlXkV8k5c8bbTbfGnZWDR1ZlbLRM9Yt5vosfwapUD90MmVkpmR3wUkO2sUKi80QfC7b4KvSDXQ+MImbGxMaU5Bnsq1PqLN95q+uat3nlAVBAELkcx51FlE9CaIS65y4J7FEDg8BE5JeuCNshh62VSYRXVSFt8bk3f/TFGgzC8OIo14BhVmiRQQ503Z1sROyf5xLX2a/EJavMm1i2Bs2TH6ROKY9z5Pz8hT5US0r381V8oG7TZyLF9HTtoy3wCYsgWA5EmLanjAsVU2YEeAA0rxzdtYP8Y2okFiJ6u+M4HQZ3Wg3peSodyp3vxdYce2vk4EKeqEFuuS82850DYb7Et7fmp+wQQUT8Q/bMO0DreWjHoMM5lE4LJ4ME6AxksmMiFtfo/4Fe2q9D+LAqZ+ANOcv9M+8Rn6ngiYmuRNd0l/a02q1PEvO6vTfXgcl4f7Z1IULHPEaDNZHCJS1K5RXYFqYQ6OHsTmOm7hnwaRAS97+VFMo1i5uvTx9nYaAcY7yzq3Ckfb67dMBKApGOpJpkvPgfrP7bgBO5rOZXM1opXqVPb09nljAhhAhyCTh1e/8+mJrBo0cLQ/LupQzVxGDgm3awSMPxsZAN45PSWz76zzxdDa1MMo51do+VJHfs7Wl0NcXAQrniOBYL9Wqt0qNkn1gY5smkkISGeQ/vxNap4MmzeZE7b5fpOy+2fpcRVQLpc4nooQzJvSVTFz+25lgZ6iHf45K87gQFMIAri1Pf/EDDpL87az+bRWvWi+BA2kMe1kf+Ay1LyMz8r+g51H0ma0bNFh6+fbWMfUiD9JCepIObclnUJ4NlWfcgHxTf17d/4tl6z4DTcLpCCk8Da77JouSHgvtcRbRlFV1OfhWZLXUsrlfpaQTiItv6TGIr3k7+7b66o3Qw/GQVs5GmYifaIZIz8n8my4XjkaMBd0SZfBzzvFjHMq6YUP9+SbjvReqofuoO+5tW1wTYZXitFFBfwuHlXm6w77K5QDBW6olT7pat41/F5eGxLcz tv@wu";
      uid = 1337; # TODO use default and document what has to be done (for vv)
    };
    tv-nomic = {
      inherit (config.krebs.users.tv) mail;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3dYR/n4Yw8OsYmfR2rSUG7o10G6AqOlSJHuHSEmANbMkqjiWl1TnpORtt5bAktyAGI4vWf9vhNEnxuIqGXWSV+3yCd7yfBHR1m0Y9QSw6blQ0xc1venl3JU0kpEyJfUn8a9cdXlnRiS0MP1gcsN7Zk8cqBELJYJajkSEnsT4eVaU5/wdnyzUO1fk8D8tFBJbF/tsWDLJPu4P18rpxq4wZgA2qmyHoVDEVlrz2OYcziXT6gpG0JGnToteaNg9ok5QavEYFpp8P+k1AacrBjc1PAb4MaMX1nfkSyaZwSqLdH35XkNRgPhVVmqZ5PlG3VeNpPSwpdcKi8P3zH1xG9g6Usx1SAyvcoAyGHdOwmFuA2tc1HgYEiQ+OsPrHZHujBOOZsKTN9+IZHScCAe+UmUcK413WEZKPs8PeFjf1gQAoDXb55JpksxLAnC/SQOl4FhkctIAXxr12ALlyt9UFPzIoj/Nj2MpFzGSlf653fTFmnMbQ8+GICc4TUpqx5GELZhfQuprBTv/55a9zKvM4B8XT3Bn9olQzMQIXEjXb3WUVFDDNWeNydToorYn1wG3ZWQ+3f0IlqRicWO7Q9BRj1Lp5rcUCb+naJ48tGY6HFUZ1Kz/0x458GDFvUd8mCJjqqmeSkUEeZd0xet5tVFg/bYoSslEqPF6pz7V3ruJMSdYxnQ== tv@nomic #2";
    };
    tv-xu = {
      inherit (config.krebs.users.tv) mail;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC/3nkqxe8YrDVt615n96A7iC3vvwsiqgpsBYC/bhwfBHu1bAtBmTWVqSKDIdwg7p8TQpIKtAgZ3IJT3BlrnVTeR4RIviLjHjYWW1NBhm+nXi+heThgi5fLciE3lVLVsy5X9Kc1ZPLgLa1In0REOanwbueOD0ESN1yKIDwUUdczw/o3dLDMzanqFHKuSSN4o9Ex2x+MRj9eLsb706s4VSYMo3lirRCJeAOGv1C7Xg1cuepdhIeJsq9aF7vSy15c0nCkWwr8zdY7pbMPYCe5zvIEymZ0UowZ5HQ3NmIZnYDxa4E1PFjDczHdQbVmmGMI80grNwMsHzQ6bynHSPXDoLf4WodXlhS0+9Ju5QavDT6uqZ9uhDBuWC8QNgWUMIJnEaTBFyA0OI1akl8Q2RLC+qnNf5IwItSq+GDwEsB2ZJNW3kOk1kNiCUrBafRYpPaFeP97wzzP4uYlBKAr2SOLrrkf7NFEdw2ihxhDMNnps/ErRJ8U0zdpmalw8mItGyqRULpHjk/wN00rYOdBIhW3G3QJuVgtGnWtGCBG5x70EfMiSEXPD3YSsVVsgKD+v8qr+YiilRRD+N3gaHhiOWA6HgxRNul/P4llk0ktTpb9LoHk2+oooTH5ZuuT/8yF8J4stZt7EIOH+mSOAXG1z0BwnEkQu7pVKwu/oOZpGJTvBrGwww== tv@xu";
    };
    vv = {
      mail = "vv@mu.r";
      uid = 2000; # TODO use default
    };
  };
}
