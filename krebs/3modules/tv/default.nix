{ config, ... }:

with import <stockholm/lib>;

{
  dns.providers = {
    "viljetic.de" = "regfish";
  };
  hosts = mapAttrs (_: setAttr "owner" config.krebs.users.tv) {
    alnus = {
      cores = 2;
      managed = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.21.1";
          ip6.addr = "42::2101";
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
        };
      };
      ssh.privkey.path = <secrets/ssh.id_rsa>;
      ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDP9JS2Nyjx4Pn+/4MrFi1EvBBYVKkGm2Q4lhgaAiSuiGLol53OSsL2KIo01mbcSSBWow9QpQpn8KDoRnT2aMLDrdTFqL20ztDLOXmtrSsz3flgCjmW4f6uOaoZF0RNjAybd1coqwSJ7EINugwoqOsg1zzN2qeIGKYFvqFIKibYFAnQ8hcksmkvPdIO5O8CbdIiP9sZSrSDp0ZyLK2T0PML2jensVZOeqSPulQDFqLsbmavpVLkpDjdzzPRwbZWNB4++YeipbYNOkX4GR1EB4wMZ93IbBV7kpJtib2Zb2AnUf7UW37hxWBjILdstj9ClwNOQggn8kD9ub7YxBzH1dz0Xd8a0mPOAWIDJz9MypXgFRc3vdvPB/W1I4Se0CLbgOkORun9CkgijKr9oEY8JNt8HFd6viZcAaQxOyIm6PNHZTnHfdSc7bIBS2n3e3IZBv0fTd77knGLXg402aTuu2bm/kxsKivxsILXIaGbeXe4ceN3Fynr3FzSM2bUkzHb0mAHu1BQ9YaX0xzCwjVueA5nzGls7ODSFkXsiBfg2FvMN/sTLFca6tnwyqcnD6nujoiS5+BxjDWPgnZYqCaW3B/IkpTsRMsX6QrfhOFcsP8qlJ2Cp82orWoDK/D0vZ9pdzAc6PFGga0RofuJKY2yiq+SRZ7/e9E6VncIVCYZ1OfN0Q==";
    };
    cd = {
      cores = 2;
      extraZones = {
        # TODO generate krebsco.de zone from nets and don't use extraZones at all
        "krebsco.de" = ''
          cd          60 IN A ${config.krebs.hosts.cd.nets.internet.ip4.addr}
        '';
      };
      managed = true;
      nets = {
        internet = {
          ip4.addr = "45.62.237.203";
          aliases = [
            "cd.i"
            "cd.krebsco.de"
          ];
          ssh.port = 11423;
        };
        retiolum = {
          via = config.krebs.hosts.cd.nets.internet;
          ip4.addr = "10.243.113.222";
          ip6.addr = "42:4522:25f8:36bb:8ccb:150:231a:2af3";
          aliases = [
            "cd.r"
            "cgit.cd.r"
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
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOd/HqZIO9Trn3eycl23GZAz21HQCISaVNfNyaLSQvJ6";
    };
    ju = {
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.144";
          aliases = [
            "ju.gg23"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.13.39";
          ip6.addr = "42::1339";
          aliases = [
            "ju.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAy2xyuOJ/G7uuXz8SfL8mkeX/YwAqnty98/h4BGHwd4ENLt2cUtim
            BUjVFIWdIMRds+4H8UtveGSeuYgRs3CpQeNuAeq20YlwoxeZgsc8mA+FP/zeN10n
            OAaP/+BTLKAHQ3Ixq41vLrFXU4Ah53YhOw1LqxQG80Tcr4J8Yehx+mrdGhcDnp2p
            4QpMLtMoAn0dQ3K5muZUQzGMHamLIril8hDKkJPqBVN0DRQ2lAVcK70AcqyuFIUM
            rWkG8gI7AT1bhZ3viIMX9wjpuA3BaitqIEyUCjWv2ZLy2HmTDGGfhEqNYdx/pXKt
            HToZk1XPnNfopFFtOHiSh1P06VqPex6MIHpbgEf7cVlxxNUOH2qssPGbo6ulzGyo
            YLeJZNP+1GxPLtyBBSiFApGdJBH8aMlQlz0vjFIdmJbIbUhSSi1TOtbQuB1SCvYO
            rp9Hm9Ah0508kxLfGlmKdQ3zO3wKbmPqCjwqSGsgtHn3KZzhgr+pTwgHIKdur1VU
            yW0vvj2ofyajgAb53cM77ScIHwbimkZ0/CbAVeM1z7OXOQ5ruXW/FVCZPe+clY2F
            ah6UOM5FdI+AYWOhkbP1EP0DGugHs5YUgTxOMMwot1TkxD/y4CZ/ctukoWZrIHHR
            vKpLhs9nWcnVXRP/trLtVl2okhs1vTYqgArgH6C0jbSXoNQbnZ+a860CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHM6dL0fQ8Bd0hER0Xa3I2pAWVHdnwOBaAZhbDlLJmUu";
    };
    kaepsele = {
      nets = {
        internet = {
          ip4.addr = "92.222.10.169";
          aliases = [
            "kaepsele.i"
            "kaepsele.internet"
            # TODO "kaepsele.org"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.166.2";
          ip6.addr = "42:b9d:6660:d07c:2bb7:4e91:1a01:2e7d";
          aliases = [
            "kaepsele.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAxj7kaye4pGLou7mVRTVgtcWFjuEosJlxVg24gM7nU1EaoRnBD93/
            Y3Je7BSUbz5xMXr5SFTPSkitInL7vU+jDOf2bEpqv+uUJAJIz85494oPS9xocdWo
            rQsrQRAtOg4MLD+YIoAxQm2Mc4nt2CSE1+UP4uXGxpuh0c051b+9Kmwv1bTyHB9y
            y01VSkDvNyHk5eA+RGDiujBAzhi35hzTlQgCJ3REOBiq4YmE1d3qpk3oNiYUcrcu
            yFzQrSRIfhXjuzIR+wxqS95HDUsewSwt9HgkjJzYF5sQZSea0/XsroFqZyTJ8iB5
            FQx2emBqB525cWKOt0f5jgyjklhozhJyiwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDA9cDUg7qm37uOhQpdKSgpnJPWao9VZR6LFNphVcJQ++gYvVgWu6WMhigiy7DcGQSStUlXkZc4HZBBugwwNWcf7aAF6ijBuG5rVwb9AFQmSexpTOfWap33iA5f+LXYFHe7iv4Pt9TYO1ga1Ryl4EGKb7ol2h5vbKC+JiGaDejB0WqhBAyrTg4tTWO8k2JT11CrlTjNVctqV0IVAMtTc/hcJcNusnoGD4ic0QGSzEMYxcIGRNvIgWmxhI6GHeaHxXWH5fv4b0OpLlDfVUsIvEo9KVozoLGm/wgLBG/tQXKaF9qVMVgOYi9sX/hDLwhRrcD2cyAlq9djo2pMARYiriXF";
    };
    mu = {
      cores = 2;
      managed = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.20.1";
          ip6.addr = "42::2001";
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
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1vJsAddvxMA84u9iJEOrIkKn7pQiemMbfW5cfK1d7g root@mu";
    };
    ni = {
      extraZones = {
        "krebsco.de" = ''
          ni          60 IN A ${config.krebs.hosts.ni.nets.internet.ip4.addr}
          cgit        60 IN A ${config.krebs.hosts.ni.nets.internet.ip4.addr}
          cgit.ni     60 IN A ${config.krebs.hosts.ni.nets.internet.ip4.addr}
          krebsco.de. 60 IN MX 5 ni
        '';
      };
      nets = {
        internet = {
          ip4.addr = "188.68.36.196";
          aliases = [
            "ni.i"
            "cgit.ni.i"
          ];
          ssh.port = 11423;
        };
        retiolum = {
          via = config.krebs.hosts.ni.nets.internet;
          ip4.addr = "10.243.113.223";
          ip6.addr = "42:4522:25f8:36bb:8ccb:150:231a:2af4";
          aliases = [
            "ni.r"
            "cgit.ni.r"
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
        };
      };
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILGDdcKwFm6udU0/x6XGGb87k9py0VlrxF54HeYu9Izb";
    };
    nomic = {
      cores = 2;
      managed = true;
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.110";
          aliases = ["nomic.gg23"];
          ssh.port = 11423;
        };
        retiolum = {
          ip4.addr = "10.243.0.110";
          ip6.addr = "42:2d5:733f:d6da:c0f5:2bb7:2b18:9ec";
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
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMIHmwXHV7E9UGuk4voVCADjlLkyygqNw054jvrsPn5t root@nomic";
    };
    ok = {
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.1";
          aliases = ["ok.gg23"];
        };
      };
    };
    schnabeldrucker = {
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.21";
          aliases = ["schnabeldrucker.gg23"];
        };
      };
    };
    schnabelscanner = {
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.22";
          aliases = ["schnabelscanner.gg23"];
        };
      };
    };
    wu = {
      cores = 4;
      managed = true;
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.37";
          aliases = [
            "wu.gg23"
            "cache.wu.gg23"
          ];
          ssh.port = 11423;
        };
        retiolum = {
          ip4.addr = "10.243.13.37";
          ip6.addr = "42::1337";
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
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcJvu8JDVzObLUtlAQg9qVugthKSfitwCljuJ5liyHa";
    };
    xu = {
      binary-cache = {
        pubkey = "xu-1:pYRENvaxZqGeImwLA9qHmRwHV4jfKaYx4u1VcZ31x0s=";
      };
      cores = 4;
      managed = true;
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.38";
          aliases = [
            "xu.gg23"
            "cache.xu.gg23"
          ];
          ssh.port = 11423;
        };
        retiolum = {
          ip4.addr = "10.243.13.38";
          ip6.addr = "42::1338";
          aliases = [
            "xu.r"
            "cgit.xu.r"
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
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPnjfceKuHNQu7S4eYFN1FqgzMqiL7haNZMh2ZLhvuhK root@xu";
    };
    zu = {
      cores = 4;
      managed = true;
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.39";
          aliases = [
            "zu.gg23"
          ];
          ssh.port = 11423;
        };
        retiolum = {
          ip4.addr = "10.243.13.40";
          ip6.addr = "42::1340";
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
      ssh.privkey.path = <secrets/ssh.id_rsa>;
      ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDNjHxyUC7afNGSwfwBfQizmDnHTNLWDRHE8SY9W4oiw2lPhCFGTN8Jz84CKtnABbZhbNY1E8T58emF2h45WzDg/OGi8DPAk4VsXSkIhyvAto+nkTy2L4atjqfvXDvqxTDC9sui+t8p5OqOK+sghe4kiy+Vx1jhnjSnkQsx9Kocu24BYTkNqYxG7uwOz6t262XYNwMn13Y2K/yygDR3Uw3wTnEjpaYnObRxxJS3iTECDzgixiQ6ewXwYNggpzO/+EfW1BTz5vmuEVf4GbQ9iEc7IsVXHhR+N0boCscvSgae9KW9MBun0A2veRFXNkkfBEMfzelz+S63oeVfelkBq6N5aLsHYYGC4VQjimScelHYVwxR7O4fV+NttJaFF7H06FJeFzPt3NYZeoPKealD5y2Muh1UnewpmkMgza9hQ9EmI4/G1fMowqeMq0U6Hu0QMDUAagyalizN97AfsllY2cs0qLNg7+zHMPwc5RgLzs73oPUsF3umz0O42I5p5733vveUlWi5IZeI8CA1ZKdpwyMXXNhIOHs8u+yGsOLfSy3RgjVKp2GjN4lfnFd0LI+p7iEsEWDRkIAvGCOFepsebyVpBjGP+Kqs10bPGpk5dMcyn9iBJejoz9ka+H9+JAG04LnXwt6Rf1CRV3VRCRX1ayZEjRv9czV7U9ZpuFQcIlVRJQ== root@zu";
    };
  };
  users = {
    dv = {
      mail = "dv@alnus.r";
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
