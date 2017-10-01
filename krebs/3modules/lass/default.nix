{ config, ... }:

with import <stockholm/lib>;

{
  hosts = mapAttrs (_: recursiveUpdate {
    owner = config.krebs.users.lass;
    ci = true;
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
          tinc.port = 993;
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGv0JMp0y+E5433GRSFKVK3cQmP0AAlS9aH9fk49yFxy";
    };
    echelon = {
      cores = 2;
      nets = rec {
        internet = {
          ip4.addr = "45.62.226.163";
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
          ip4.addr = "46.4.114.247";
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
            "cache.prism.r"
            "cgit.prism.r"
            "paste.r"
            "p.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIECgKCBAEAtpI0+jz2deUiH18T/+JcRshQi7lq8zlRvaXpvyuxJlYCz+o5cLje
            fxrKn67JbDb0cTAiDkI88alHBd8xeq2I6+CY90NT6PNVfsQBFx2v5YXafELXJWlo
            rBvPFrR7nt1VzmG/hzkY8RwgC8hC6jRn7cvWWPCkvm2ZnNtYqAjiYMcUcWv6Vn9Z
            ytPgkebDF9KpD8bL4vQu9iPZGNZpwncCw/Ix66oyTM6e24j/fTYgp7xn28wVUzUB
            wWDH0uMQOxyBGFutEvAQ48XZ+QQxZv+2ZGqWJ+MeXreUPNP5wTxFCQOrkR1EXNio
            /jgdHXtU5wVvqPwziukwwnfGJYUUHw7mjdo6ps5rch/aDxs0lahNc2TMbhr3rqgA
            BkXVfwDTt8W/PB6Z0Y/djXOlUmQKO39OgZuhsYzqM4Uj17up7CDY77SiQYrV901C
            9CR5oFsAvV+WIMFUBc7ZZGPotJ9nZ2yyLQh+fT3sXuqFpGlyaI2SAm2edZUXKWQ5
            Q6AIyQRPkTNRCDuvXxIMdmOE++tBnyCI/Psn/Qet5gFcSsUMPhto8Yaka4SgJfyu
            3iIojFUzskowLWt6dBOGm5brI/OaKz0gyw5K3Hb4T7Jz+EwoeJfhbdZYA6NIY+qH
            TGGl+47ffT+8e+1hvcAnO+bN5Br8WPN3+VD4FQD5yTb6pCFdZuL3QEyoKc9eugDb
            g/+rFOsI8bfVeH5zZrl6B6XJBLGeKEECf3zwE2JObO3IuwxATSkahx1jAEy+hFyZ
            kPwooGj03tkgVGc2AxgdHbfmNUbSVkO+m+ouBojikSrnFNKRTS/wZ69RVg3tl4qg
            7F4Vs/aMQ9bSWycvRBZQXITPQ1Y6mCEUj2mSKVHmgy/5rqwz2va/Yc1zhUptcINo
            7ztGiEzFMPGagkTs/Ntuqh2VbC/MwTao0BKl+gyCNwrACnNW87X4og2gtG3ukduz
            cnSupO84hdTrclthsSEH/rLUauBsuIch58S/F7KCz9hwK45+Btky7Kz4mf/pE451
            k88QfDHw/cTSzlESPnEnthrRnhxn0fW7FRwJpieKm2AmyEEjSiiYt8mUdD3teKj0
            dgYrcGQkCnhmKDawgcw46wstBG/sAKT8qnZPRmlzKpcCS186ffuobQvj42LSmuMu
            ToANi5pw2yEfzwLxNG/3whozB9rqwbqV/YAR/mthMxD0IXpLDKXlV1IeD7MfpV8i
            jx6SghnkX/s2F7UTOlwJYe/Gl1biLRB8EPnOZKadHR0BRWFd+Qz6pJDp0B13jT3/
            AEPNGXLwVjmdhy2TVec3OGL/CukPEdiW1Urw5lfOc9dacTXjTNTXzod7Ub6s7ZOE
            T7Y4dsVeW4OM7NmE/riqS3cG9obGWO7gIQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAsANFdMi825qWQXQbWLYuNZ6/fARt3lnh1KStQHQQMD";
    };
    archprism = rec {
      cores = 4;
      nets = rec {
        retiolum = {
          via = internet;
          ip4.addr = "10.243.0.104";
          ip6.addr = "42::fa17";
          aliases = [
            "archprism.r"
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
        internet = {
          ip4.addr = "213.239.205.240";
          aliases = [
            "archprism.i"
          ];
          ssh.port = 45621;
        };
      };
      ssh.privkey.path = <secrets/ssh.id_rsa>;
      ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChm4sqQ2bUZj+2YnTf6G5HHRTpSe1jTUhJRnwcYPYZKF+CBqBncipRpuGlGXEsptNa+7ZMcQC0ySsz5SUOMt3Ih+NehVe/qt3VtRz0l0MgOWmH2qBwKK9Y4IuxrJQzUmP4UGlOGlFj9DORssSMOyFIG4eZ9k2qMn3xal0NVRfGTShKlouWsiUILZ8I+sDNE00z8DAYesgc1yazvRnjzvLkRxdNdpYiAFBbmXMpPKK95McRJaWsuNSeal9kd5p5PagWcgN4DZ6+ebzz3NKnmzk4j+vuHX0U9lTXBqKMlzzmM2YNLRtDPfrtJNyHqLpZUpFhJKqZCD+4/0zdrzRfC7Th+5czzUCSvHiKPVsqw5eOdiQX6EyzNAF5zpkpRp//QdUNNXC5/Ku6GKCO491+TuA8VCha0fOwBONccTLUI/hGNmCh88mLbukVoeGJrbYNCOA/6kEz7ZLEveU4i+TT7okhDElMsNk+AWCZ8/NdJQNX3/K6+JJ9qAn+/yC8LdjgYYJ2oU/aw5/HyOgiQ0z4n9UfQ7j+nHysY9CQb1b3guX7yjJoc3KpNXCXEztuIRHjFD1EP8NRTSmGjsa/VjLmTLSsqjD+7IE5mT0tO5RJvmagDgdJSr/iR5D9zjW7hx7ttvektrlp9g0v3CiCFVaW4l95hGYT0HaNBLJ5R0YHm0lD+Q==";
    };
    domsen-nas = {
      ci = false;
      external = true;
      nets = rec {
        internet = {
          aliases = [
            "domsen-nas.internet"
          ];
          ip4.addr = "87.138.180.167";
          ssh.port = 2223;
        };
      };
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
          ip6.addr = "42:0:0:0:0:0:01ca:1205";
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
    skynet = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.133.116";
          ip6.addr = "42:0:0:0:0:0:0:1101";
          aliases = [
            "skynet.r"
            "cgit.skynet.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEArNpBoTs7MoaZq2edGJLYUjmoLa5ZtXhOFBHjS1KtQ3hMtWkcqpYX
            Ic457utOSGxTE+90yXXez2DD9llJMMyd+O06lHJ7CxtbJGBNr3jwoUZVCdBuuo5B
            p9XfhXU9l9fUsbc1+a/cDjPBhQv8Uqmc6tOX+52H1aqZsa4W50c9Dv5vjsHgxCB0
            yiUd2MrKptCQTdmMM9Mf0XWKPPOuwpHpxaomlrpUz07LisFVGGHCflOvj5PAy8Da
            NC+AfNgR/76yfuYWcv4NPo9acjD9AIftS2c0tD3szyHBCGaYK/atKzIoBbFbOtMb
            mwG3B0X3UdphkqGDGsvT+66Kcv2jnKwL0wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEB/MmASvx3i09DY1xFVM5jOhZRZA8rMRqtf8bCIkC+t";
    };
    helios = {
      cores = 8;
      nets = {
        retiolum = {
          ip4.addr = "10.243.133.117";
          ip6.addr = "42:0:0:0:0:0:3:7105";
          aliases = [
            "helios.r"
            "cgit.helios.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAp+SRmP5MoCSYInx4Dm5MLZzNyXVgfo/CDoeUlUT35X0yE7WHGWsG
            wHPCu+3RWfBUjuqNdb0qiGtRi3Q/LwznwBROPOX8gMXia/DgCLbIjn5Rx081pTIo
            3epbUCFtNgyDWg8IHF87ZnVBXTYAy5g4tz9u8kw82D8mR18o595TuZ9t5pDc/Kvi
            fPHZenT6cd6FtL9uankX/jan1PRP9xTrhpE8dAQ6g+7XH7knMK3cno/Ztis5YzHt
            Ith0bsIjk5of7hhITj0MXtTikjDqWxkpF5mfOK1cG/rC1goTmB9AfcENUBnu9iAM
            I/alzqk3CEczznLyaOckfx2fRuar912LAdiJ5v7VPztfvN1p3gIxq5M0Rgkq+98B
            H/s32xNRBPvqoIleKnhwE9gfrCLaAVqpaMkgKRvgsTkSDNYNhh4smQ3eAKKwwDH/
            QG3sfP8xyNyDFhBtCiDGkf9hNqBBMaKjZoh8DasZNtcfOop3fGw7jmUUbB6cG8cp
            +EfYbcb5mVpmrIyXgOTwwYcp7tn+zkd4Wa8C9Q98eFTs0HGVGxGX9Hj6PM/kXK4C
            aIqIQVNpnJ/9cOwT8JFIriG1MWTOXbamUusKTLs8SRp3ZkyM7XUEcLL5HMh09rUw
            rzEAmE7TywXVhd7j2IaEy+bx2dfGQH2bFoh6Drm6Olo+ySi1utB5dGkCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIqpx9jJnn4QMGO8BOrGOLRN1rgpIkR14sQb8S+otWEL";
    };
    iso = {
      ci = false;
      cores = 1;
    };
    sokrateslaptop = {
      ci = false;
      external = true;
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
    };
    turingmachine = {
      ci = false;
      external = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.168";
          ip6.addr = "42:4992:6a6d:600::1";
          aliases = [
            "turingmachine.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAxh+5HD1oAFTvMWEra2pYrA3HF8T4EnkP917lIUiuN7xUj7sawu0C
            t1/1IfIlH9dbxgFe5CD/gXvokxHdovPTGVH11L+thZgq6hg/xbYvZAl76yLxj7t9
            6+Ocac08TQZYMqWKShz5jqTVE/DLz4Cdy0Qk9sMJ1++OmH8jsWgK5BkogF99Gwf8
            ZiI0t3n3lCZsm3v592lveDcVIh6hjuCIvFVxc+7cOj0MKm1LxLWbCHZlUIE3he4g
            nZu4XiYaE4Y2LicMs8zKehnQkkXrP1amT56SqUfbSnWR+HZc2+KjwRDI5BPeTS06
            5WHwkQs0ScOn7vFZci3rElIc7vilu2eKGF1VLce9kXw9SU2RFciqavaEUXbwPnwT
            1WF35Ct+qIOP0rXoObm6mrsj7hJnlBPlVpb58/kTxLHMSHPzqQRbFZ35f6tZodJ1
            gRMKKEnMX8/VWm6TqLUIpFCCTZ5PH1fxaAnulHCxksK03UyfUOvExCTU4x8KS9fl
            DIoLlV9PFBlAW8mTuIgRKYtHacsc31/5Tehcx0If09NuMFT9Qfl2/Q3p6QJomRFL
            W5SCP9wx2ONhvZUkRbeihBiTN5/h3DepjOeNWd1DvE6K0Ag8SXMyBGtyKfer4ykW
            OR0iCiRQQ5QBmNuJrBLRUyfoPqFUXBATT1SrRj8vzXO1TjTmANEMFD0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    eddie = {
      ci = false;
      external = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.170";
          ip6.addr = "42:4992:6a6d:700::1";
          aliases = [ "eddie.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAuRQphRlSIC/aqRTfvStPdJOJCx1ACeFIDEjRdgoxuu32qoBl7i6d
            j7Voh+Msditf2a5+f0fVsNDaPnjPGfk0NkZBjmn+RZQDRXk0krpTNj2Vb6W5quTm
            3yrjJMFJR9CU5khfppc47X+ir8bjn7RusWTFNEuDvUswHmRmnJHLS3Y+utOaRbCF
            2hxpyxCn423gpsaBfORPEK8X90nPbuNpFDugWPnC+R45TpNmIf4qyKvfhd9OKrua
            KNanGHG30xhBW/DclUwwWi8D44d94xFnIRVcG1O+Uto93WoUWZn90lI1qywSj5Aq
            iWstBK4tc7VwvAj0UzPlaRYYPfFjOEkPQzj8xC6l/leJcgxkup252uo6m1njMx3t
            6QWMgevjqosY22OZReZfIwb14aDWFKLTWs30J+zmWK4TjlRITdsOEKxlpODMbJAD
            kfSoPwuwkWIzFhNOrFiD/NtKaRYmV8bTBCT3a9cvvObshJx13BP+IUFzBS1N1n/u
            hJWYH5WFsQZn/8rHDwZGkS1zKPEaNoydjqCZNyJpJ5nhggyl6gpuD7wpXM/8tFay
            pAjRP40+qRQLUWXmswV0hsZTOX1tvZs4f68y3WJ+GwCWw9HvvwmzYes5ayJrPsbJ
            lyK301Jb42wGEsVWxu3Eo/PLtp8OdD+Wdh6o/ELcc0k/YCUGFMujUM8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    borg = {
      ci = false;
      external = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.171";
          ip6.addr = "42:4992:6a6d:700::2";
          aliases = [ "borg.r" ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA0bHZApTM7Hl4qqNakSwq
            bt7zJoTVK9ePoC3Mue1VmJ1mCKMaxKdzlO31kPeHtkilAzgyIJdgikyKFlApGsQL
            aIuU9h55X7TbikoDD6ghbSrAe3Pgc+sJ3OZ7wO7Qb8CKgJvEbkk/u68YiJgyTjYD
            HNjIQzlsGdpoSke9vwC8qWanfgN7c2MMGtakqfXDjYjCgp7O43i+SMupkMSXIXMA
            5XUFh/vVp6xgPxBofcw0uQIyZ5v4PPFjnGPm4rnMbFzbhubntHjDadwGd5Niyw4O
            zNNKNchTLfNiuNGqTZeYd0kJ5fNMKykhpSs+ou34MvexvpuyPlFuotnPXN/nOMml
            3nwiqzthzPuBZRLswxT0WvlA8wlbeTOKJ0wTIR4dDuAF+euDtoNocVEN5PJNc7yN
            fmwAV6geESoJbZQMSCtAp1NioaBlRPp1pFfoM/GotHywuFrTIxyoIBiYhkpWyQvq
            WYw5j13IKqkL7jDchhoBmcardmh+AP5bL3uQ84BgaYNwFzHp04qIRrrdpF0eMaHB
            /8zaqsNLn4/zQJB5ffkelwoIqfvLPQeCMLzHGHgP5xUnWgmZZGiiDLvhuaMeNq4U
            EpCKoTL178sPOgNfHfd8mEqx0qKYuPrNQEdlpa5xOZqwx56pfYpGWY+KtF2FHLhS
            iO64GCJqCi1MKBYx/NhaxKMCAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
    };
    inspector = {
      ci = false;
      external = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.172";
          ip6.addr = "42:4992:6a6d:800::1";
          aliases = [ "inspector.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAr3l/u7qcxmFa2hUICU3oPDhB2ij2R3lKHyjSsVFVLNfl6TpOdppG
            EDXOapeXL0s+PfBRHdRI3v/dibj4PG9eyKmFxsUJ2gRz4ghb1UE23aQ3pkr3x8sZ
            7GR+nJYATYf+jolFF9O1x+f0Uo5xaYWkGOMH8wVVzm6+kcsZOYuTEbJAsbTRZywF
            m1MdRfk54hLiDsj2rjGRZIR+ZfUKVs2MTWOLCpBAHLJK+r3HfUiR2nAgeNkJCFLw
            WIir1ftDIViT3Ly6b7enaOkVZ695FNYdPWFZCE4AJI0s9wsbMClzUqCl+0mUkumd
            eRXgWXkmvBsxR4GECnxUhxs6U8Wh3kbQavvemt4vcIKNhkw32+toYc1AFK/n4G03
            OUJBbRqgJYx9wIvo8PEu4DTTdsPlQZnMwiaKsn+Gi4Ap6JAnG/iLN8sChoQf7Dau
            ARZA3sf9CkKx5sZ+9dVrLbzGynKE18Z/ysvf1BLd/rVVOps1B/YRBxDwPj8MZJ0x
            B7b0j+hRVV5palp3RRdcExuWaBrMQQGsXwLUZOFHJJaZUHF9XRdy+5XVJdNOArkG
            q1+yGhosL1DLTQE/VwCxmBHyYTr3L7yZ2lSaeWdIeYvcRvouDROUjREVFrQjdqwj
            7vIP1cvDxSSqA07h/xEC4YZKACBYc/PI2mqYK5dvAUG3mGrEsjHktPUCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
  };
  users = {
    lass = {
      mail = "lass@mors.r";
      pubkey = builtins.readFile ./ssh/mors.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/mors.pgp;
    };
    lass-helios = {
      mail = "lass@helios.r";
      pubkey = builtins.readFile ./ssh/helios.rsa;
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
    archprism-repo-sync = {
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINR9oL/OPHjjKjQ+IyRqWpgrXdZrKKAwFKIte8gYml6C";
      mail = "lass@prism.r";
    };
    prism-repo-sync = {
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKhpCKTnSq6VDJPB+0NiHu2ZxSKEIxHN6uPAPnbXYNCe";
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
    Mic92 = {
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE";
      mail = "joerg@higgsboson.tk";
    };
  };
}
