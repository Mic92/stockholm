{ config, ... }:

with import <stockholm/lib>;

{
  hosts = mapAttrs (_: recursiveUpdate {
    owner = config.krebs.users.lass;
    managed = true;
  }) {
    dishfire = {
      cores = 4;
      nets = rec {
        internet = {
          ip4.addr = "144.76.172.188";
          aliases = [
            "dishfire.i"
          ];
          ssh.port = 45621;
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.133.99";
          ip6.addr = "42:0000:0000:0000:0000:0000:d15f:1233";
          aliases = [
            "dishfire.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAwKi49fN+0s5Cze6JThM7f7lj4da27PSJ/3w3tDFPvtQco11ksNLs
            Xd3qPaQIgmcNVCR06aexae3bBeTx9y3qHvKqZVE1nCtRlRyqy1LVKSj15J1D7yz7
            uS6u/BSZiCzmdZwu3Fq5qqoK0nfzWe/NKEDWNa5l4Mz/BZQyI/hbOpn6UfFD0LpK
            R4jzc9Dbk/IFNAvwb5yrgEYtwBzlXzeDvHW2JcPq3qQjK2byQYNiIyV3g0GHppEd
            vDbIPDFhTn3Hv5zz/lX+/We8izzRge7MEd+Vn9Jwb5NAzwDsOHl6ExpqASv9H49U
            HwgPw5pstabyrsDWXybSYUb+8LcZf+unGwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGv0JMp0y+E5433GRSFKVK3cQmP0AAlS9aH9fk49yFxy";
    };
    echelon = {
      cores = 2;
      nets = rec {
        internet = {
          ip4.addr = "104.233.79.118";
          aliases = [
            "echelon.i"
          ];
          ssh.port = 45621;
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.206.103";
          ip6.addr = "42:941e:2816:35f4:5c5e:206b:3f0b:f763";
          aliases = [
            "echelon.r"
            "cgit.echelon.r"
            "go.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAuscWOYdHu0bpWacvwTNd6bcmrAQ0YFxJWHZF8kPZr+bMKIhnXLkJ
            oJheENIM6CA9lQQQFUxh2P2pxZavW5rgVlJxIKeiB+MB4v6ZO60LmZgpCsWGD/dX
            MipM2tLtQxYhvLJIJxEBWn3rxIgeEnCtZsH1KLWyLczb+QpvTjMJ4TNh1nEBPE/f
            4LUH1JHaGhcaHl2dLemR9wnnDIjmSj0ENJp2al+hWnIggcA/Zp0e4b86Oqbbs5wA
            n++n5j971cTrBdA89nJDYOEtepisglScVRbgLqJG81lDA+n24RWFynn+U3oD/L8p
            do+kxlwZUEDRbPU4AO5L+UeIbimsuIfXiQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL21QDOEFdODFh6WAfNp6odrXo15pEsDQuGJfMu/cKzK";
    };
    prism = rec {
      cores = 4;
      extraZones = {
        "krebsco.de" = ''
          prism     IN A ${nets.internet.ip4.addr}
          paste     IN A ${nets.internet.ip4.addr}
        '';
      };
      nets = rec {
        internet = {
          ip4.addr = "213.239.205.240";
          aliases = [
            "prism.i"
            "paste.i"
          ];
          ssh.port = 45621;
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.0.103";
          ip6.addr = "42:0000:0000:0000:0000:0000:0000:15ab";
          aliases = [
            "prism.r"
            "build.prism.r"
            "cgit.prism.r"
            "cache.prism.r"
            "paste.r"
            "p.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAvzhoBsxUaEwm7ctiw3xvLFP2RoVaiHnF+Sm4J8E4DOerPToXxlyl
            kxvMPaRnhtiO6MK0Vv2+VswKIeRkMm5YuD5MG7wni4vUKcRx9cCgKji/s0vGqLhl
            JKK9i23q7epvQ32Is/e3P+fQ5KM50EO+TWACNaroCNoyJvZ/G8BWXw6WnIOsuX0I
            AoPW2ol8/sdZxeK4hCe/aQz6y0AEvigpvPkHx+TE5fkBeIeqhiKTIWpEqjU4wXx5
            jP2izYuaIsHAihU8mm03xRxT4+4IHYt6ddrhNeBuJBsATLkDgULdQyOoEzmXCm2j
            anGRBZoYVazxn7d8mKBdE09ZNc1ijULZgwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_rsa>;
      ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChm4sqQ2bUZj+2YnTf6G5HHRTpSe1jTUhJRnwcYPYZKF+CBqBncipRpuGlGXEsptNa+7ZMcQC0ySsz5SUOMt3Ih+NehVe/qt3VtRz0l0MgOWmH2qBwKK9Y4IuxrJQzUmP4UGlOGlFj9DORssSMOyFIG4eZ9k2qMn3xal0NVRfGTShKlouWsiUILZ8I+sDNE00z8DAYesgc1yazvRnjzvLkRxdNdpYiAFBbmXMpPKK95McRJaWsuNSeal9kd5p5PagWcgN4DZ6+ebzz3NKnmzk4j+vuHX0U9lTXBqKMlzzmM2YNLRtDPfrtJNyHqLpZUpFhJKqZCD+4/0zdrzRfC7Th+5czzUCSvHiKPVsqw5eOdiQX6EyzNAF5zpkpRp//QdUNNXC5/Ku6GKCO491+TuA8VCha0fOwBONccTLUI/hGNmCh88mLbukVoeGJrbYNCOA/6kEz7ZLEveU4i+TT7okhDElMsNk+AWCZ8/NdJQNX3/K6+JJ9qAn+/yC8LdjgYYJ2oU/aw5/HyOgiQ0z4n9UfQ7j+nHysY9CQb1b3guX7yjJoc3KpNXCXEztuIRHjFD1EP8NRTSmGjsa/VjLmTLSsqjD+7IE5mT0tO5RJvmagDgdJSr/iR5D9zjW7hx7ttvektrlp9g0v3CiCFVaW4l95hGYT0HaNBLJ5R0YHm0lD+Q==";
    };
    domsen-nas = {
      nets = rec {
        internet = {
          aliases = [
            "domsen-nas.internet"
          ];
          ip4.addr = "87.138.180.167";
          ssh.port = 2223;
        };
      };
      managed = false;
    };
    cloudkrebs = {
      cores = 1;
      nets = rec {
        internet = {
          ip4.addr = "104.167.113.104";
          aliases = [
            "cloudkrebs.i"
          ];
          ssh.port = 45621;
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.206.102";
          ip6.addr = "42:941e:2816:35f4:5c5e:206b:3f0b:f762";
          aliases = [
            "cloudkrebs.r"
            "cgit.cloudkrebs.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAttUygCu7G6lIA9y+9rfTpLKIy2UgNDglUVoKZYLs8JPjtAtQVbtA
            OcWwwPc8ijLQvwJWa8e/shqSzSIrtOe+HJbRGdXLdBLtOuLKpz+ZFHcS+95RS5aF
            QTehg+QY7pvhbrrwKX936tkMR568suTQG6C8qNC/5jWYO/wIxFMhnQ2iRRKQOq1v
            3aGGPC16KeXKVioY9KoV98S3n1rZW1JK07CIsZU4qb5txtLlW6FplJ7UmhVku1WC
            sgOOj9yi6Zk1t8R2Pwv9gxa3Hc270voj5U+I2hgLV/LjheE8yhQgYHEA4vXerPdO
            TGSATlSmMtE2NYGrKsLM7pKn286aSpXinwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN7oYx7Lbkc0wPYNp92LQF93DCtxsGzOkVD91FJQzVZl";
    };
    uriel = {
      cores = 1;
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.12";
          aliases = ["uriel.gg23"];
          ssh.port = 45621;
        };
        retiolum = {
          ip4.addr = "10.243.81.176";
          ip6.addr = "42:dc25:60cf:94ef:759b:d2b6:98a9:2e56";
          aliases = [
            "uriel.r"
            "cgit.uriel.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAzw0pvoEmqeqiZrzSOPH0IT99gr1rrvMZbvabXoU4MAiVgGoGrkmR
            duJkk8Fj12ftMc+Of1gnwDkFhRcfAKOeH1RSc4CTircWVq99WyecTwEZoaR/goQb
            MND022kIBoG6NQNxv1Y5I1B/h7hfloMFEPym9oFtOAXoGhBY2vVl4g64NNz+RLME
            m1RipLXKANAh6LRNPGPQCUYX4TVY2ZJVxM3CM1XdomUAdOYXJmWFyUg9NcIKaacx
            uRrmuy7J9yFBcihZX5Y7NV361kINrpRmZYxJRf9cr0hb5EkJJ7bMIKQMEFQ5RnYo
            u7MPGKD7aNHa6hLLCeIfJ5u0igVmSLh3pwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBryIo/Waw8SWvlQ0+5I+Bd/dJgcMd6iPXtELS6gQXoc";
      secure = true;
    };
    mors = {
      cores = 2;
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.11";
          aliases = ["mors.gg23"];
          ssh.port = 45621;
        };
        retiolum = {
          ip4.addr = "10.243.0.2";
          ip6.addr = "42:0:0:0:0:0:0:dea7";
          aliases = [
            "mors.r"
            "cgit.mors.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAsj1PCibKOfF68gmFQ+wwyfhUWpqKqpznrJX1dZ+daae7l7nBHvsE
            H0QwkiMmk3aZy1beq3quM6gX13aT+/wMfWnLyuvT11T5C9JEf/IS91STpM2BRN+R
            +P/DhbuDcW4UsdEe6uwQDGEJbXRN5ZA7GI0bmcYcwHJ9SQmW5v7P9Z3oZ+09hMD+
            1cZ3HkPN7weSdMLMPpUpmzCsI92cXGW0xRC4iBEt1ZeBwjkLCRsBFBGcUMuKWwVa
            9sovca0q3DUar+kikEKVrVy26rZUlGuBLobMetDGioSawWkRSxVlfZvTHjAK5JzU
            O6y6hj0yQ1sp6W2JjU8ntDHf63aM71dB9QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINAMPlIG+6u75GJ3kvsPF6OoIZsU+u8ZQ+rdviv5fNMD";
    };
    shodan = {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.4";
          ip6.addr = "42:0:0:0:0:0:0:50d4";
          aliases = [
            "shodan.r"
            "cgit.shodan.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA9bUSItw8rEu2Cm2+3IGHyRxopre9lqpFjZNG2QTnjXkZ97QlDesT
            YYZgM2lBkYcDN3/LdGaFFKrQQSGiF90oXA2wFqPuIfycx+1+TENGCzF8pExwbTd7
            ROSVnISbghXYDgr3TqkjpPmnM+piFKymMDBGhxWuy1bw1AUfvRzhQwPAvtjB4VvF
            7AVN/Z9dAZ/LLmYfYq7fL8V7PzQNvR+f5DP6+Eubx0xCuyuo63bWuGgp3pqKupx4
            xsixtMQPuqMBvOUo0SBCCPa9a+6I8dSwqAmKWM5BhmNlNCRDi37mH/m96av7SIiZ
            V29hwypVnmLoJEFiDzPMCdiH9wJNpHuHuQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC9vup68R0I+62FK+8LNtwM90V9P4ukBmU7G7d54wf4C";
    };
    icarus = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.133.114";
          ip6.addr = "42:0000:0000:0000:0000:0000:d15f:1214";
          aliases = [
            "icarus.r"
            "cgit.icarus.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAydCY+IWzF8DocCNzPiUM+xccbiDTWS/+r2le812+O4r+sUojXuzr
            Q4CeN+pi2SZHEOiRm3jO8sOkGlv4I1WGs/nOu5Beb4/8wFH6wbm4cqXTqH/qFwCK
            7+9Bke8TUaoDj9E4ol9eyOx6u8Cto3ZRAUi6m1ilrfs1szFGS5ZX7mxI73uhki6t
            k6Zb5sa9G8WLcLPIN7tk3Nd0kofd/smwxSN0mXoTgbAf1DZ3Fnkgox/M5VnwpPW7
            zLzbWNFyLIgDGbQ5vZBlJW7c4O0KrMlftvEQ80GeZXaKNt6UK7LSAQ4Njn+8sXTt
            gl0Dx29bSPU3L8udj0Vu6ul7CiQ5bZzUCQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOPgQIMYiyD4/Co+nlOQWEzCKssemOEXAY/lbIZZaMhj";
    };
    daedalus = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.133.115";
          ip6.addr = "42:0:0:0:0:0:daed:a105";
          aliases = [
            "daedalus.r"
            "cgit.daedalus.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAzlIJfYIoQGXishIQGFNOcaVoeelqy7a731FJ+VfrqeR8WURQ6D+8
            5hz7go+l3Z7IhTc/HbpGFJ5QJJNFSuSpLfZVyi+cKAUVheTivIniHFIRw37JbJ4+
            qWTlVe3uvOiZ0cA9S6LrbzqAUTLbH0JlWj36mvGIPICDr9YSEkIUKbenxjJlIpX8
            ECEBm8RU1aq3PUo/cVjmpqircynVJBbRCXZiHoxyLXNmh23d0fCPCabEYWhJhgaR
            arkYRls5A14HGMI52F3ehnhED3k0mU8/lb4OzYgk34FjuZGmyRWIfrEKnqL4Uu2w
            3pmEvswG1WYG/3+YE80C5OpCE4BUKAzYSwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAq5Ovdcsljr5dOl7+2sQNKpGpdX0SlOIuCZKEiWEp8g";
    };
    iso = {
      cores = 1;
      managed = false;
    };
    sokrateslaptop = {
      nets = {
        retiolum = {
          ip4.addr = "10.243.142.104";
          ip6.addr = "42:f8a1:044d:0f75:9d73:56d8:f432:c6cc";
          aliases = [
            "sokrateslaptop.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA0EMbBv5NCSns4V/VR/NJHhwe2qNLUYjWWtCDY4zDuoiJdm3JNZJ2
            t0iKNxFwd6Mmg3ahAlndsH4FOjOBGBQCgBG25VRnQgli1sypI/gYTsSgIWHVIRoZ
            rgrng0K3oyJ6FuTP+nH1rd7UAYkrOQolXQBY+LqAbxOVjiJl+DpbAXIxCIs5TBeW
            egtBiXZ1S53Lv5EGFXug716XlgZLHjw7PzRLJXSlvUAIRZj0Sjq4UD9VrhazM9s5
            aDuxJIdknccEEXm6NK7a51hU/o8L+T0IUpZxhaXOdi6fvO/y3TbffKb1yRTbN0/V
            VBjBh18Le7h0SmAEED5tz7NOCrAjMZQtJQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      managed = false;
    };
  };
  users = {
    lass = {
      mail = "lass@mors.r";
      pubkey = builtins.readFile ./ssh/mors.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/mors.pgp;
    };
    lass-uriel = {
      mail = "lass@uriel.r";
      pubkey = builtins.readFile ./ssh/uriel.rsa;
    };
    lass-shodan = {
      mail = "lass@shodan.r";
      pubkey = builtins.readFile ./ssh/shodan.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/shodan.pgp;
    };
    lass-icarus = {
      mail = "lass@icarus.r";
      pubkey = builtins.readFile ./ssh/icarus.rsa;
    };
    fritz = {
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCz34435NSXgj72YAOL4cIlRq/4yInKEyL9no+gymURoW5x1nkYpP0EK331e7UyQQSOdWOogRo6d7YHcFqNlYWv5xlYcHucIhgJwC4Zda1liVA+v7tSOJz2BjmFvOT3/qlcPS69f3zdLHZooz2C33uHX1FgGRXlxiA8dpqGnSr8o76QLZjuQkuDqr8reOspjO/RHCo2Moq0Xm5q9OgN1WLAZzupqt9A5lx567mRzYsRAr23pUxVN8T/tSCgDlPe4ktEjYX9CXLKfMyh9WuBVi+AuH4GFEWBT+AMpsHeF45w+w956x56mz0F5nYOQNK87gFr+Jr+mh2AF1ot2CxzrfTb fritz@scriptkiddiT540";
    };
    prism-repo-sync = {
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINR9oL/OPHjjKjQ+IyRqWpgrXdZrKKAwFKIte8gYml6C";
      mail = "lass@prism.r";
    };
    mors-repo-sync = {
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGv6N/UjFnX5vUicT9Sw0+3x4mR0760iaVWZ/JDtdV4h";
      mail = "lass@mors.r";
    };
    sokratess = {
    };
    wine-mors = {
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEKfTIKmbe1RjX1fjAn//08363zAsI0CijWnaYyAC842";
    };
  };
}
