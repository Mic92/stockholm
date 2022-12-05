with import ../../lib;
{ config, ... }: let

  r6 = ip: (krebs.genipv6 "retiolum" "lass" ip).address;
  w6 = ip: (krebs.genipv6 "wiregrill" "lass" ip).address;

in {
  dns.providers = {
    "lassul.us" = "zones";
  };
  hosts = mapAttrs (_: recursiveUpdate {
    owner = config.krebs.users.lass;
    consul = true;
    ci = true;
    monitoring = true;
  }) {
    dishfire = {
      cores = 4;
      nets = rec {
        internet = {
          ip4 = rec {
            addr = "157.90.232.92";
            prefix = "${addr}/32";
          };
          aliases = [
            "dishfire.i"
          ];
          ssh.port = 45621;
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.133.99";
          ip6.addr = r6 "d15f:1233";
          aliases = [
            "dishfire.r"
            "grafana.lass.r"
            "prometheus.lass.r"
            "alert.lass.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEAwKi49fN+0s5Cze6JThM7f7lj4da27PSJ/3w3tDFPvtQco11ksNLs
              Xd3qPaQIgmcNVCR06aexae3bBeTx9y3qHvKqZVE1nCtRlRyqy1LVKSj15J1D7yz7
              uS6u/BSZiCzmdZwu3Fq5qqoK0nfzWe/NKEDWNa5l4Mz/BZQyI/hbOpn6UfFD0LpK
              R4jzc9Dbk/IFNAvwb5yrgEYtwBzlXzeDvHW2JcPq3qQjK2byQYNiIyV3g0GHppEd
              vDbIPDFhTn3Hv5zz/lX+/We8izzRge7MEd+Vn9Jwb5NAzwDsOHl6ExpqASv9H49U
              HwgPw5pstabyrsDWXybSYUb+8LcZf+unGwIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
            pubkey_ed25519 = "P+bhzhgTNdohWdec//t/e+8cI7zUOsS+Kq/AOtineAO";
          };
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGv0JMp0y+E5433GRSFKVK3cQmP0AAlS9aH9fk49yFxy";
    };
    prism = rec {
      cores = 4;
      extraZones = {
        "krebsco.de" = ''
          cache     IN A ${nets.internet.ip4.addr}
          p         IN A ${nets.internet.ip4.addr}
          c         IN A ${nets.internet.ip4.addr}
          paste     IN A ${nets.internet.ip4.addr}
          prism     IN A ${nets.internet.ip4.addr}
        '';
        "lassul.us" = ''
          $TTL 3600
          @ IN SOA dns16.ovh.net. tech.ovh.net. (2017093001 86400 3600 3600000 300)
                              60 IN NS     ns16.ovh.net.
                              60 IN NS     dns16.ovh.net.
                              60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
                              60 IN AAAA   ${config.krebs.hosts.prism.nets.internet.ip6.addr}
                                 IN MX     5 mail.lassul.us.
                              60 IN TXT    "v=spf1 mx -all"
                              60 IN TXT    ( "v=DKIM1; k=rsa; t=s; s=*; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDUv3DMndFellqu208feABEzT/PskOfTSdJCOF/HELBR0PHnbBeRoeHEm9XAcOe/Mz2t/ysgZ6JFXeFxCtoM5fG20brUMRzsVRxb9Ur5cEvOYuuRrbChYcKa+fopu8pYrlrqXD3miHISoy6ErukIYCRpXWUJHi1TlNQhLWFYqAaywIDAQAB" )
          default._domainkey  60 IN TXT    "k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDUv3DMndFellqu208feABEzT/PskOfTSdJCOF/HELBR0PHnbBeRoeHEm9XAcOe/Mz2t/ysgZ6JFXeFxCtoM5fG20brUMRzsVRxb9Ur5cEvOYuuRrbChYcKa+fopu8pYrlrqXD3miHISoy6ErukIYCRpXWUJHi1TlNQhLWFYqAaywIDAQAB"
          cache               60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          cgit                60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          pad                 60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          codi                60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          go                  60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          io                  60 IN NS     ions.lassul.us.
          ions                60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          lol                 60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          matrix              60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          paste               60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          radio               60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          jitsi               60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          streaming           60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          mumble              60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          mail                60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          flix                60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          confusion           60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          testing             60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
        '';
      };
      nets = rec {
        internet = {
          ip4 = {
            addr = "95.216.1.150";
            prefix = "0.0.0.0/0";
          };
          ip6 = {
            addr = "2a01:4f9:2a:1e9::1";
            prefix = "2a01:4f9:2a:1e9::/64";
          };
          aliases = [
            "prism.i"
            "paste.i"
          ];
          ssh.port = 45621;
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.0.103";
          ip6.addr = r6 "1";
          aliases = [
            "prism.r"
            "cache.prism.r"
            "cgit.prism.r"
            "bota.r"
            "flix.r"
            "jelly.r"
            "paste.r"
            "c.r"
            "p.r"
            "search.r"
            "radio-news.r"
          ];
          tinc = {
            pubkey = ''
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
            pubkey_ed25519 = "XbBBPg+dtZM1LRN46VAujVKIC6VSo6nFoHo/1unbggO";
          };
        };
        wiregrill = {
          via = internet;
          ip4.addr = "10.244.1.103";
          ip6.addr = w6 "1";
          aliases = [
            "prism.w"
          ];
          wireguard = {
            pubkey = "oKJotppdEJqQBjrqrommEUPw+VFryvEvNJr/WikXohk=";
            subnets = [
              (krebs.genipv6 "wiregrill" "external" 0).subnetCIDR
              (krebs.genipv6 "wiregrill" "lass" 0).subnetCIDR
              "10.244.1.0/24"
            ];
          };
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAsANFdMi825qWQXQbWLYuNZ6/fARt3lnh1KStQHQQMD";
      syncthing.id = "QITFKYQ-VEPIPL2-AZIXHMD-BBT62ML-YHSB35A-BSUIBXS-QYMPFHW-M7XN2QU";
    };
    mors = {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.2";
          ip6.addr = r6 "dea7";
          aliases = [
            "mors.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEAsj1PCibKOfF68gmFQ+wwyfhUWpqKqpznrJX1dZ+daae7l7nBHvsE
              H0QwkiMmk3aZy1beq3quM6gX13aT+/wMfWnLyuvT11T5C9JEf/IS91STpM2BRN+R
              +P/DhbuDcW4UsdEe6uwQDGEJbXRN5ZA7GI0bmcYcwHJ9SQmW5v7P9Z3oZ+09hMD+
              1cZ3HkPN7weSdMLMPpUpmzCsI92cXGW0xRC4iBEt1ZeBwjkLCRsBFBGcUMuKWwVa
              9sovca0q3DUar+kikEKVrVy26rZUlGuBLobMetDGioSawWkRSxVlfZvTHjAK5JzU
              O6y6hj0yQ1sp6W2JjU8ntDHf63aM71dB9QIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
            pubkey_ed25519 = "kuh0cP/HjGOQ+NafR3zjmqp+RAnA59F4CgtzENj9/MM";
          };
        };
        wiregrill = {
          ip6.addr = w6 "dea7";
          aliases = [
            "mors.w"
          ];
          wireguard.pubkey = "FkcxMathQzJYwuJBli/nibh0C0kHe9/T2xU0za3J3SQ=";
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINAMPlIG+6u75GJ3kvsPF6OoIZsU+u8ZQ+rdviv5fNMD";
      syncthing.id = "ZPRS57K-YK32ROQ-7A6MRAV-VOYXQ3I-CQCXISZ-C5PCV2A-GSFLG3I-K7UGGAH";
    };
    shodan = {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.4";
          ip6.addr = r6 "50da";
          aliases = [
            "shodan.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEA9bUSItw8rEu2Cm2+3IGHyRxopre9lqpFjZNG2QTnjXkZ97QlDesT
              YYZgM2lBkYcDN3/LdGaFFKrQQSGiF90oXA2wFqPuIfycx+1+TENGCzF8pExwbTd7
              ROSVnISbghXYDgr3TqkjpPmnM+piFKymMDBGhxWuy1bw1AUfvRzhQwPAvtjB4VvF
              7AVN/Z9dAZ/LLmYfYq7fL8V7PzQNvR+f5DP6+Eubx0xCuyuo63bWuGgp3pqKupx4
              xsixtMQPuqMBvOUo0SBCCPa9a+6I8dSwqAmKWM5BhmNlNCRDi37mH/m96av7SIiZ
              V29hwypVnmLoJEFiDzPMCdiH9wJNpHuHuQIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
            pubkey_ed25519 = "Ptc5VuYkRd5+zHibZwNe3DEgGHHvAk0Ul00dW1YXsrC";
          };
        };
        wiregrill = {
          ip6.addr = w6 "50da";
          ip4.addr = "10.244.1.4";
          aliases = [
            "shodan.w"
          ];
          wireguard.pubkey = "0rI/I8FYQ3Pba7fQ9oyvtP4a54GWsPa+3zAiGIuyV30=";
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC9vup68R0I+62FK+8LNtwM90V9P4ukBmU7G7d54wf4C";
      syncthing.id = "AU5RTWC-HXNMDRT-TN4ZHXY-JMQ6EQB-4ZPOZL7-AICZMCZ-LNS2XXQ-DGTI2Q6";
    };
    icarus = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.133.114";
          ip6.addr = r6 "1205";
          aliases = [
            "icarus.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEAydCY+IWzF8DocCNzPiUM+xccbiDTWS/+r2le812+O4r+sUojXuzr
              Q4CeN+pi2SZHEOiRm3jO8sOkGlv4I1WGs/nOu5Beb4/8wFH6wbm4cqXTqH/qFwCK
              7+9Bke8TUaoDj9E4ol9eyOx6u8Cto3ZRAUi6m1ilrfs1szFGS5ZX7mxI73uhki6t
              k6Zb5sa9G8WLcLPIN7tk3Nd0kofd/smwxSN0mXoTgbAf1DZ3Fnkgox/M5VnwpPW7
              zLzbWNFyLIgDGbQ5vZBlJW7c4O0KrMlftvEQ80GeZXaKNt6UK7LSAQ4Njn+8sXTt
              gl0Dx29bSPU3L8udj0Vu6ul7CiQ5bZzUCQIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
            pubkey_ed25519 = "vUc/ynOlNqB7a+sr0BmfdRv0dATtGZTjsU2qL2yGInK";
          };
        };
        wiregrill = {
          ip6.addr = w6 "1205";
          aliases = [
            "icarus.w"
          ];
          wireguard.pubkey = "mVe3YdlWOlVF5+YD5vgNha3s03dv6elmNVsARtPLXQQ=";
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOPgQIMYiyD4/Co+nlOQWEzCKssemOEXAY/lbIZZaMhj";
      syncthing.id = "7V75LMM-MIFCAIZ-TAWR3AI-OXONVZR-TEW4GBK-URKPPN4-PQFG653-LGHPDQ4";
    };
    daedalus = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.133.115";
          ip6.addr = r6 "daed";
          aliases = [
            "daedalus.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEAzlIJfYIoQGXishIQGFNOcaVoeelqy7a731FJ+VfrqeR8WURQ6D+8
              5hz7go+l3Z7IhTc/HbpGFJ5QJJNFSuSpLfZVyi+cKAUVheTivIniHFIRw37JbJ4+
              qWTlVe3uvOiZ0cA9S6LrbzqAUTLbH0JlWj36mvGIPICDr9YSEkIUKbenxjJlIpX8
              ECEBm8RU1aq3PUo/cVjmpqircynVJBbRCXZiHoxyLXNmh23d0fCPCabEYWhJhgaR
              arkYRls5A14HGMI52F3ehnhED3k0mU8/lb4OzYgk34FjuZGmyRWIfrEKnqL4Uu2w
              3pmEvswG1WYG/3+YE80C5OpCE4BUKAzYSwIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
            pubkey_ed25519 = "ybmNcRLtZ0NxlxIRE3bdc2G4lLXtTGXu+iRaXMTKCNG";
          };
        };
        wiregrill = {
          ip6.addr = w6 "daed";
          aliases = [
            "daedalus.w"
          ];
          wireguard.pubkey = "ZVTTWbJfe8Oq6E6QW1qgXU91FnkuKDGJO3MF3I3gDFI=";
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAq5Ovdcsljr5dOl7+2sQNKpGpdX0SlOIuCZKEiWEp8g";
    };
    skynet = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.133.116";
          ip6.addr = r6 "5ce7";
          aliases = [
            "skynet.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEArNpBoTs7MoaZq2edGJLYUjmoLa5ZtXhOFBHjS1KtQ3hMtWkcqpYX
              Ic457utOSGxTE+90yXXez2DD9llJMMyd+O06lHJ7CxtbJGBNr3jwoUZVCdBuuo5B
              p9XfhXU9l9fUsbc1+a/cDjPBhQv8Uqmc6tOX+52H1aqZsa4W50c9Dv5vjsHgxCB0
              yiUd2MrKptCQTdmMM9Mf0XWKPPOuwpHpxaomlrpUz07LisFVGGHCflOvj5PAy8Da
              NC+AfNgR/76yfuYWcv4NPo9acjD9AIftS2c0tD3szyHBCGaYK/atKzIoBbFbOtMb
              mwG3B0X3UdphkqGDGsvT+66Kcv2jnKwL0wIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
            pubkey_ed25519 = "9s7eB16k7eAtHyneffTCmYR7s3mRpJqpVVjSPGaVKKN";
          };
        };
        wiregrill = {
          ip6.addr = w6 "5ce7";
          aliases = [
            "skynet.w"
          ];
          wireguard.pubkey = "pt9a6nP+YPqxnSskcM9NqRmAmFzbO5bE7wzViFFonnU=";
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEB/MmASvx3i09DY1xFVM5jOhZRZA8rMRqtf8bCIkC+t";
      syncthing.id = "KWGPAHH-H53Y2WL-SDAUVQE-7PMYRVP-6Q2INYB-FL535EO-HIE7425-ZCNP7A3";
    };
    littleT = {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.133.77";
          ip6.addr = r6 "771e";
          aliases = [
            "littleT.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIECgKCBAEA2nPi6ui8nJhEL3lFzDoPelFbEwFWqPnQa0uVxLAhf2WnmT/vximF
              /m2ZWpKDZyKx17GXQwm8n0NgyvcemvoCVGqSHIsbxvLB6aBF6ZLkeKyx1mZioEDY
              1MWR+yr42dFn+6uVTxJhLPmOxgX0D3pWe31UycoAMSWf4eAhmFIEFUvQCAW43arO
              ni1TFSsaHOCxOaLVd/r7tSO0aT72WbOat84zWccwBZXvpqt/V6/o1MGB28JwZ92G
              sBMjsCsoiciSg9aAzMCdjOYdM+RSwHEHI9xMineJgZFAbQqwTvK9axyvleJvgaWR
              M9906r/17tlqJ/hZ0IwA6X+OT4w/JNGruy/5phxHvZmDgvXmYD9hf2a6JmjOMPp/
              Zn6zYCDYgSYugwJ7GI39GG7f+3Xpmre87O6g6WSaMWCfdOaAeYnj+glP5+YvTLpT
              +cdN9HweV27wShRozJAqTGZbD0Nfs+EXd0J/q6kP43lwv6wyZdmXCShPF2NzBlEY
              xdtWKhRYKC1cs0Z2nK+XGEyznNzp1f8NC5qvTguj4kDMhoOd6WXwk460HF49Tf/c
              aGQTGzgEVMAI7phTJubEmxdBooedvPFamS5wpHTmOt9dZ3qbpCgThaMblVvUu/lm
              7pkPgc60Y2RAk/Rvyy5A8AaxBXPRBNwVkM5TY/5TW+S1zY09600ZCC2GE27qGT9v
              k4GHabO42n3wTHk+APodzKDBbEazhOp5Oclg4nNKqgg+IrmheB91oEqBXlfyDj8B
              idVoUvbH9WPwBqdh7hoqzrHDur5wCFBphrkjEe98o5iFFFi2C8W04H7iqe+nFqvJ
              y/vzKk5kbfpjov71EEje+hNUCLTWF7sjgT4Z2z8LuqjpIq+d2i5dASfTqj4VBs6D
              SeiHyyAfCHG/03I9E5eizCCd98Tr30yhu3IKsdFFXsVwxHVFenq2Y1ca7uypCk+i
              mDC5q5WQFEK/8SSO25i1teWBawfNVVVI/A1b676VJyafS9ebJs8TmXYRbE6rcBzH
              PssdHNwbtEwhbGdQhgQ2pqQg1SIZM3zvjcpgzL9QP29tulubJ05keaw/4p/Yg/mB
              ivF8EAIefXYYVxYkRQsHox7UQpSCzjOtj7gvc0KdJxshSLuryM0LxP+gk+x6JPX5
              Ht8x+oE7iL0cqBsIenc/e0XdTZ+4zrBY5hWbGH8a8VJqEYs54WRJhzQf1jzNaCbS
              8328MpRF5lXujv61aveg0i4pvczznlSV7wXmmwNAdhvSUTh34tCpRqabpCJdlRBt
              NvVuij6guPKt4XV1TxXNsPCfib1vYjvwX8gUE4UhL69VmM8OBaC3XdroMfNvz9YW
              5ObxDGIEiP53Jp8hiWId0AI/XF5Ct3Gh2wIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
            pubkey_ed25519 = "rDnc4Ha+M6fyN5JU4lkV9NKfMBtIHOcG4/AUB9KodiP";
          };
        };
        wiregrill = {
          ip6.addr = w6 "771e";
          aliases = [
            "littleT.w"
          ];
          wireguard.pubkey = "VfSTPO1XGqLqujAGCov1yA0WxyRXJndZCW5XYkScNXg=";
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJzb9BPFClubs6wSOi/ivqPFVPlowXwAxBS0jHaB29hX";
      syncthing.id = "PCDXICO-GMGWKSB-V6CYF3I-LQMZSGV-B7YBJXA-DVO7KXN-TFCSQXW-XY6WNQD";
    };
    xerxes = {
      cores = 2;
      consul = false;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.1.3";
          ip6.addr = r6 "3";
          aliases = [
            "xerxes.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIECgKCBAEArqEaK+m7WZe/9/Vbc+qx2TjkkRJ9lDgDMr1dvj98xb8/EveUME6U
              MZyAqNjLuKq3CKzJLo02ZmdFs4CT1Hj28p5IC0wLUWn53hrqdy8cCJDvIiKIv+Jk
              gItsxJyMnRtsdDbB6IFJ08D5ReGdAFJT5lqpN0DZuNC6UQRxzUK5fwKYVVzVX2+W
              /EZzEPe5XbE69V/Op2XJ2G6byg9KjOzNJyJxyjwVco7OXn1OBNp94NXoFrUO7kxb
              mTNnh3D+iB4c3qv8woLhmb+Uh/9MbXS14QrSf85ou4kfUjb5gdhjIlzz+jfA/6XO
              X4t86uv8L5IzrhSGb0TmhrIh5HhUmSKT4RdHJom0LB7EASMR2ZY9AqIG11XmXuhj
              +2b5INBZSj8Cotv5aoRXiPSaOd7bw7lklYe4ZxAU+avXot9K3/4XVLmi6Wa6Okim
              hz+MEYjW5gXY+YSUWXOR4o24jTmDjQJpdL83eKwLVAtbrE7TcVszHX6zfMoQZ5M9
              3EtOkDMxhC+WfkL+DLQAURhgcPTZoaj0cAlvpb0TELZESwTBI09jh/IBMXHBZwI4
              H1gOD5YENpf0yUbLjVu4p82Qly10y58XFnUmYay0EnEgdPOOVViovGEqTiAHMmm5
              JixtwJDz7a6Prb+owIg27/eE1/E6hpfXpU8U83qDYGkIJazLnufy32MTFE4T9fI4
              hS8icFcNlsobZp+1pB3YK4GV5BnvMwOIVXVlP8yMCRTDRWZ4oYmAZ5apD7OXyNwe
              SUP2mCNNlQCqyjRsxj5S1lZQRy1sLQztU5Sff4xYNK+5aPgJACmvSi3uaJAxBloo
              4xCCYzxhaBlvwVISJXZTq76VSPybeQ+pmSZFMleNnWOstvevLFeOoH2Is0Ioi1Fe
              vnu5r0D0VYsb746wyRooiEuOAjBmni8X/je6Vwr1gb/WZfZ23EwYpGyakJdxLNv3
              Li+LD9vUfOR80WL608sUU45tAx1RAy6QcH/YDtdClbOdK53+cQVTsYnCvDW8uGlO
              scQWgk+od3qvo6yCPO7pRlEd3nedcPSGh/KjBHao6eP+bsVERp733Vb9qrEVwmxv
              jlZ1m12V63wHVu9uMAGi9MhK+2Q/l7uLTj03OYpi4NYKL2Bu01VXfoxuauuZLdIJ
              Z3ZV+qUcjzZI0PBlGxubq6CqVFoSB7nhHUbcdPQ66WUnwoKq0cKmE7VOlJQvJ07u
              /Wsl8BIsxODVt0rTzEAx0hTd5mJCX7sCawRt+NF+1DZizl9ouebNMkNlsEAg4Ps0
              bQerZLcOmpYjGa5+lWDwJIMXVIcxwTmQR86stlP/KQm0vdOvH2ZUWTXcYvCYlHkQ
              sgVnnA2wt+7UpZnEBHy04ry+jYaSsPdYgwIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
            pubkey_ed25519 = "PRtxFg/zw8dmwEGEM+u28N5GWuGNiHSNlaieplVSqQK";
          };
        };
        wiregrill = {
          ip6.addr = w6 "3";
          aliases = [
            "xerxes.w"
          ];
          wireguard.pubkey = "UTm8B8YUVvBGqwwxAUMVFsVQFQGQ6jbcXAavZ8LxYT8=";
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE5HyLyaIvVH0qHIQ4ciKhDiElhSqsK+uXcA6lTvL+5n";
      syncthing.id = "EA76ZHP-DF2I3CJ-NNTFEUH-YGPQK5S-T7FQ6JA-BNQQUNC-GF2YL46-CKOZCQM";
    };
    yellow = {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.14";
          ip6.addr = r6 "3110";
          aliases = [
            "yellow.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN PUBLIC KEY-----
              MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA6lHmzq8+04h3zivJmIbP
              MkYiW7KflcTWQrl/4jJ7DVFbrtS6BSSI0wIibW5ygtLrp2nYgWv1jhg7K9q8tWMY
              b6tDv/ze02ywCwStbjytW3ymSZUJlRkK2DQ4Ld7JEyKmLQIjxXYah+2P3QeUxLfU
              Uwk6vSRuTlcb94rLFOrCUDRy1cZC73ZmtdbEP2UZz3ey6beo3l/K5O4OOz+lNXgd
              OXPls4CeNm6NYhSGTBomS/zZBzGqb+4sOtLSPraNQuc75ZVpT8nFa/7tLVytWCOP
              vWglPTJOyQSygSoVwGU9I8pq8xF1aTE72hLGHprIJAGgQE9rmS9/3mbiGLVZpny6
              C6Q9t6vkYBRb+jg3WozIXdUvPP19qTEFaeb08kAuf1xhjZhirfDQjI7K6SFaDOUp
              Y/ZmCrCuaevifaXYza/lM+4qhPXmh82WD5ONOhX0Di98HBtij2lybIRUG/io4DAU
              52rrNAhRvMkUTBRlGG6LPC4q6khjuYgo9uley5BbyWWbCB1A9DUfbc6KfLUuxSwg
              zLybZs/SHgXw+pJSXNgFJTYGv1i/1YQdpnbTgW4QsEp05gb+gA9/6+IjSIJdJE3p
              DSZGcJz3gNSR1vETk8I2sSC/N8wlYXYV7wxQvSlQsehfEPrFtXM65k3RWzAAbNIJ
              Akz4E3+xLVIMqKmHaGWi0usCAwEAAQ==
              -----END PUBLIC KEY-----
            '';
            pubkey_ed25519 = "qZBhDSW6ir1/w6lOngg2feCZj9W9AfifEMlKXcOb5QE";
          };
        };
        wiregrill = {
          ip6.addr = w6 "3110";
          aliases = [
            "yellow.w"
          ];
          wireguard.pubkey = "YeWbR3mW+nOVBE7bcNSzF5fjj9ppd8OGHBJqERAUVxU=";
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC03TCO73NQZHo7NKZiVJp2iiUbe6PQP14Kg3Bnlkqje ";
    };
    blue = {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.77";
          ip6.addr = r6 "b1ce";
          aliases = [
            "blue.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN PUBLIC KEY-----
              MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA28b+WMiQaWbwUPcJlacd
              QwyX4PvVm9WItPmmNy+RE2y0Mf04LxZ7RLm5+e0wPuhXXQyhZ06CNd6tjeaKfXUc
              sNeC1Vjuh1hsyYJLR5Xf/YRNJQKoaHjbkXGt+rSK7PPuCcsUPOSZSEAgHYVvcFzM
              wWE4kTDcBZeISB4+yLmPIZXhnDImRRMEurFNRiocoMmEIu/zyYVq8rnlTl972Agu
              PMGo1HqVxCouEWstRvtX5tJmV8yruRbH4tADAruLXErLLwUAx/AYDNRjY1TYYetJ
              RoaxejmZVVIvR+hWaDLkHZO89+to6wS5IVChs1anFxMNN6Chq2v8Bb2Nyy1oG/H/
              HzXxj1Rn7CN9es5Wl0UX4h9Zg+hfspoI75lQ509GLusYOyFwgmFF02eMpxgHBiWm
              khSJzPkFdYJKUKaZI0nQEGGsFJOe/Se5jj70x3Q5XEuUoQqyahAqwQIYh6uwhbuP
              49RBPHpE+ry6smhUPLTitrRsqeBU4RZRNsUAYyCbwyAH1i+K3Q5PSovgPtlHVr2N
              w+VZCzsrtOY2fxXw0e+mncrx/Qga62s4m6a/dyukA5RytA9f6bBsvSTqr7/EQTs6
              ZEBoPudk7ULNEbfjmJtBkeG7wKIlpgzVg/JaCAwMuSgVjrpIHrZmjOVvmOwB8W6J
              Ch/o7chVljAwW4JmyRnhZbMCAwEAAQ==
              -----END PUBLIC KEY-----
            '';
            pubkey_ed25519 = "vf3JzuLpEkjcwZtuJ/0M9Zjfp5ChKXvkORMXsZ4nJKL";
          };
        };
        wiregrill = {
          ip6.addr = w6 "b1ce";
          aliases = [
            "blue.w"
          ];
          wireguard.pubkey = "emftvx8v8GdoKe68MFVL53QZ187Ei0zhMmvosU1sr3U=";
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILSBxtPf8yJfzzI7/iYpoRSc/TT+zYmE/HM9XWS3MZlv";
      syncthing.id = "J2LMIPD-PBEPVKL-A3MN6NQ-KL6DZ4N-K4GGWZB-E2EPLFN-PDLVAOC-DCSZHAD";
    };

    green = {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.66";
          ip6.addr = r6 "12ee";
          aliases = [
            "green.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN PUBLIC KEY-----
              MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAwpgFxMxWQ0Cp3I82bLWk
              uoDBjWqhM9Pgq6PJSpJjyNAgMkKJcQnWi0WpELaHISAVqjdPGUQSLiar++JN3YBx
              ZQGFiucG0ijVJKAUbQQDYbc+RGK8MGO2v3Bv/6E56UKjxtT1zjjvkyXpSC7FN477
              n9IfsvIzH/RLcAP5VnHBYqZ467UR4rqi7T7yWjrEgr+VirY9Opp9LM9YozlbRrlI
              hYshk5RET/EvOSwYlw/KJEMMmYHro74neZKIVKoXD3CSE66rncNmdFwD3ZXVxYn6
              m3Eob8ojWPW+CpAL2AurUyq4Igem9JVigZiyKGgaYsdkOWgkYLW2M0DXX+vCRcM6
              BvJgJn7s0PHkLvybEVveTolRWO+I/IG1LN8m0SvrVPXf5JYHB32nKYwVMLwi+BQ1
              pwo0USGByVRv2lWZfy3doKxow0ppilq4DwoT+iqVO4sK5YhPipBHSmCcaxlquHjy
              2k1eb0gYisp0LBjHlhTErXtt4RlrUqs/84RfgtIZYUowJfXbtEbyDmLIlESbY7qk
              UlXIMXtY0sWpDivWwpdMj9kJdKlS09QTMeLYz4fFGXMksFmLijx8RKDOYfNWL7oA
              udmEOHPzYzu/Ex8RfKJjD4GhWLDvDTcyXDG9vmuDNZGcPHANeg23sGhr5Hz37FRT
              3MVh92sFyMVYkJcL7SISk80CAwEAAQ==
              -----END PUBLIC KEY-----
            '';
            pubkey_ed25519 = "WfH8ULtWklOFK6htphdSSL46vHn6TkJIhsvK9fK+4+C";
          };
        };
        wiregrill = {
          ip6.addr = w6 "12ee";
          aliases = [
            "green.w"
          ];
          wireguard.pubkey = "lOORkStNJ6iP5ffqjHa/kWOxilJIMW4E6BEtNvNhLGk=";
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH0wqzo7rMkyw6gqTGuUp8aUA0vtwj0HuuaTIkkOnA30 ";
      syncthing.id = "CADHN7J-CWRCWTZ-3GZRLII-JBVZN4N-RGHDGDL-UTAJNYI-RZPHK55-7EYAWQM";
    };

    massulus = {
      cores = 1;
      ci = false;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.113";
          ip6.addr = r6 "113";
          aliases = [
            "massulus.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN PUBLIC KEY-----
              MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEApwYalnJ2E1e3WOttPCpt
              ypNm2adUXS/pejcbF68oRvgv6NRMOKVkoFVEzdnCLYTkYkwcpGd+oRO91F+ekZrN
              ndEoicuzHNyG6NTXfW3Sjj9Au/NoAVwOJxAztzXMBAsH5pi4PSiqIQZC4l6cyv2K
              zUNm1LvW5Z5/W0J5XCUw3/B4Py7V/HjW9Yxe8MCaCVVP2kF5SwjmfQ+Yp+8csvU3
              F30xFjcTJjjWUPSkubgxtsfkrbbjzdMZhKldi3l9LhbYWD8O4bUTrTau/Emaaf6e
              v5paVh9Kczwg7Ugk9Co3GL4tKOE2I7kRQV2Rg0M5NcRBUwfxkl6JTI2PmY0fNmYd
              kdLQ1fKlFOrkyHuPBjZET1UniomlLpdycyyZii+YWLoQNj4JlFl8nAlPbqkiy8EF
              LcHvB2VfdjjyBY25TtYPjFzFsEYKd8HQ7djs8rvJvmhu4tLDD6NaOqJPWMo7I7rW
              EavQWZd+CELCJNN8eJhYWIGpnq+BI00FKayUAX+OSObYCHD1AikiiIaSjfDCrCJb
              KVDj/uczOjxHk6TUVbepFA7C8EAxZ01sgHtUDkIfvcDMs4DGn88PmjPW+V/4MfKl
              oqT7aVv6BYJdSK63rH3Iw+qTvdtzj+vcoO+HmRt2I2Be4ZPSeDrt+riaLycrVF00
              yFmvsQgi48/0ZSwaVGR8lFUCAwEAAQ==
              -----END PUBLIC KEY-----
            '';
            pubkey_ed25519 = "QwKNyv97Q2/fmPrVkgbGIhDTVW+uKu+F2enGCtZJgkM";
            port = 1655;
          };
        };
        wiregrill = {
          ip6.addr = w6 "113";
          aliases = [
            "massulus.w"
          ];
          wireguard.pubkey = ''
            4wXpuDBEJS8J1bxS4paz/eZP1MuMfgHDCvOPn4TYtHQ=
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKH8lFXZ/d2NtqyrpslTGRNBR7FJZCJ6i3UPy0LDl9t7 ";
    };

    phone = {
      consul = false;
      nets = {
        wiregrill = {
          ip4.addr = "10.244.1.13";
          ip6.addr = w6 "a";
          aliases = [
            "phone.w"
          ];
          wireguard.pubkey = "FY4PB8E/RC2JvtLgq/IDyMmZ9Ln6pz6eGyoytmUFMgk=";
        };
      };
      external = true;
      ci = false;
      syncthing.id = "PWKVXPB-JCNO6E4-KVIQ7CK-6FSOWHM-AWORMDU-HVVYLKW-44DQTYW-XZT7DQJ";
    };
    tablet = {
      consul = false;
      nets = {
        wiregrill = {
          ip4.addr = "10.244.1.14";
          ip6.addr = w6 "b";
          aliases = [
            "tablet.w"
          ];
          wireguard.pubkey = "eIafsxYEFCqmWNFon6ZsYXeDrK4X1UJ9KD0zmNZjgEI=";
        };
      };
      external = true;
      ci = false;
    };
    hilum = {
      consul = false;
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.20.123";
          ip6.addr = r6 "005b";
          aliases = [
            "hilum.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN PUBLIC KEY-----
              MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAul1zLdJ76kIqVWjxT2bb
              pLx6gu6VycxaDcWAoTWSjPsOT2IJf3NYC6i8D6WASnRqR6djp06OG7Onu0r5hZhi
              V5nelDUvR75qVAx9ZeuQDSdNpWuVMds/C3cQM6QQHD1kFwnr2n6VH/qy0W9duW8c
              SGX3C80nRpmY0cCEEnxFdFdLSd0c15M+lFVAaqh2225ujXyyvkwH874yvpWLPSdh
              4xjZdrOFarl5yb9q83HcZsdunn+469BeKCWB8bs+nRsp9Wwj1en1yAZTB3WazYNE
              saFQ0xGa7VGfHN0PjqgZEF2I2IiQJ+H3N5XRQ7dcJzsDRB8lMrCx2ynJkJRSjLXz
              vgZjW+Rf47V9CLRjJGCp1xh6GbXqjsIYh5yqZkgH4Sm1VpMBYdr/kLjiygwzV8jY
              8uoBUgEHLc5B73/D3GlMe3bOJmxxMfyPITVTFHgznycalBNBSsgKpIwWae6LbYhZ
              wrpi66IQOyC6YYThqn8pz3KUz17HxyacA/mS6/jcRP+IiHb9CYcS4BsjTpH3NnM3
              RkSWE3FGE+ULH1W/VeA8pZRKAR1rypvMRdewbFTQpe/dNgif5O5Fe/7l/6KDzzCh
              Zqqr6sEFhutPUd6PcaVtQlfzYkJ9MGYWYr4S17D7Q9V0H37a0AcRaYH59FCmlFjl
              87b8jfJNXlKFW+EBxBxN2uECAwEAAQ==
              -----END PUBLIC KEY-----
            '';
            pubkey_ed25519 = "9D50r3DmftSe2L++jPktQRbcCrE4sEazMewgbQbodRH";
          };
        };
        wiregrill = {
          ip6.addr = w6 "005b";
          aliases = [
            "hilum.w"
          ];
          wireguard.pubkey = ''
            0DRcCDR0O+UqV07DsGfS4On+6YaZ3LPfvni9u1NZNhw=
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPARXXe0HaP1r0pLqtInhnbYSZsP0g4VC6aaWP7qi5+w";
      syncthing.id = "J6PHKTS-2JG5NOL-H5ZWOF6-6L6ENA7-L4RO6DV-BQHU7YL-CHOLDCC-S5YX3AC";
    };
    styx = {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.11.1";
          ip6.addr = r6 "111";
          aliases = [
            "styx.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN PUBLIC KEY-----
              MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAuMJFklzpbxoDGD8LQ3tn
              ETYrLu/TJjq5iSQx/JbbonJriMS3X/0+m8JREzeol67svQDuZEXTEg5EfEldxrrU
              aZpNmTSmFbj2NLLCIfNBL/oLOvg9ElzhN+f+4jvakfEKi7Y7LekV25VVGrHbOEVE
              3G6XWfHx5qO5Vd6kqNWQKD3LG38aZ/Lx9XYDMbujYxPGCtOsabtAz8BKo/RgOZzi
              6A/54RFhdecJm0VoQk3iKpp2YqyCN6dLfJVLil4cREs4sW6nDyF4Y4l3dtZdfskq
              m/MoZt6fwOjNIKuI9DGdU4/X1hQelnemstzxY5x1XwG52cz+ww0h7pMF2aggsHqn
              Vmaq3b0fXrbn066Ybkbhz3UEIU9zKQGYaANGCnXxbvkd5lWbIN60GEXGE3zYJSAt
              EH3FLDTGa27fTNgAnbdnSV40KWKN4FM0iY/xrt3aOXfneTP9S2fqzTVEL9vd04C/
              7RWvRjvZ7mlAi+kVKSHkOibFVjeo+Z4Pvw5YxCAavrjXCiWj8zP8o3MNWcq/bMao
              Uk9zBMXymm8zX43w5LNnhf59oitBjiY/mzZ3NDI9N3szMvJsaUEnhO4Kq1CWtMs2
              6/TpEyRSmen1UmNwgKKFx3rELuctwMmNbOLL8cGLotEBhIk7vnZKD7NvLVX7xtOF
              wzhy2N6a3ypB4XqM7dBzzAUCAwEAAQ==
              -----END PUBLIC KEY-----
            '';
            pubkey_ed25519 = "yVT5nQstw+o5P0ZoBK81G7sL6nQEBwg42wyBn6ogZgK";
          };
        };
        wiregrill = {
          ip6.addr = w6 "111";
          aliases = [
            "styx.w"
          ];
          wireguard.pubkey = ''
            0BZfd8f0pZMRfyoHrdYZY0cR5zfFvJcS8gQLn6xGuFs=
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII3OpzRB3382d7c2apdHC+U/R0ZlaWxXZa3GFAj54ZhU ";
      syncthing.id = "JAVJ6ON-WLCWOA3-YB7EHPX-VGIN4XF-635NIVZ-WZ4HN4M-QRMLT4N-5PL5MQN";
    };

    coaxmetal = {
      cores = 16;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.17";
          ip6.addr = r6 "17";
          aliases = [
            "coaxmetal.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN PUBLIC KEY-----
              MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAwcuMl/W6DZ7UMK4RHrxA
              xCc8CkqpUTYldPdB9KJmcH6OpbQqCcPxGOvRe42NdOfCyy11WjAjUMRGnzMyi4MK
              gMEjcrl5CnQd9nF9f8Mom8cuSOVm1j46qY7Trl/MsEKsKHiYAHtLFpHz2+UI+HBU
              WbSeDLLA8g79SZq/pqWHfp3YKzqP4p+dmi8j+aOZJWkGu9l+Q40qQrTJQCxYgEek
              ODeBFCY3DGfJRn79IFGuhF1/jGiAwF3/1j2Rxlesazl6/Lyvmtioplsqn8J94z32
              G5wyGpqn/BcXkJTlWtwb3Rrg6OOALJAqy2H5EoIVT26gwmvkEStMtvgLfAeYjL8F
              G2bAtaeQGzwQZNuVJAMI9Qtb+PHw322Wz+P8U669C/HCdGCumMf+M7UDHP79kXOO
              IFs1NvkU3z/iO/5bj41v8u0W8+b9NWe++dI8N8q0hWLPgnz5PI998xW06Dul7pAX
              K1OMIMfTTGgAZHAF1Kdn1BSXezgwkutwzy5h8XkYclyHB2nPXkXIYmahi1XgWeAE
              7B4NmefbS6H8dLOU7yMEWuxmYl41UOybtyrsp1za5wtERpQgzl6EWfIXISEdx1Ly
              bmb3SGtB85RyqqCe2O9DzVZCw7mXgN69R5efyEuq3HIIN9udLNrybPNNyD/OlAqo
              l/xwDxiSCEsO6yY5lGc0MCMCAwEAAQ==
              -----END PUBLIC KEY-----
            '';
            pubkey_ed25519 = "bEGgA5Wupw+Dgh6Ub7V21Y3wOmyspW1rKGrZsVhi3cO";
          };
        };
        wiregrill = {
          ip6.addr = w6 "17";
          aliases = [
            "coaxmetal.w"
          ];
          wireguard.pubkey = ''
            lkjR14oOVKl03/0sUzOmddf28ps+v5qRxrbRY03Pg38=
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO9vAYuTv07c9bOjDJId3ShXJ1qIEuyrjkVYkJn9yMET ";
      syncthing.id = "W5BJ4TL-GAQ46WS-ZB72HFS-XOURLBA-RNBVMYC-POFH4UA-CBORQID-BMIHNQZ";
    };

    echelon = {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.3";
          ip6.addr = r6 "4";
          aliases = [
            "echelon.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN PUBLIC KEY-----
              MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEArxTpl0YvJWiF9cAYeAdp
              1gG18vrSeYDpmVCsZmxi2qyeWNM4JGSVPYoagyKHSDGH60xvktRh/1Zat+1hHR0A
              MAjDIENn9hAICQ8lafnm2v3+xzLNoTMJTYG3eba2MlJpAH0rYP0E5xBhQj9DCSAe
              UpEZWAwCKDCOmg/9h0gvs3kh0HopwjOE1IEzApgg05Yuhna96IATVdBAC7uF768V
              rJZNkQRvhetGxB459C58uMdcRK3degU6HMpZIXjJk6bqkzKBMm7C3lsAfaWulfez
              gavFSHC15NbHkz+fcVZNZReJhfTHP7k05xo5vYpDhszdUSjc3MtWBmk5v9zdS1pO
              c+20a1eurr1EPoYBqjQL0tLBwuQc2tN5XqJKVY5LGAnojAI6ktPKPLR6qZHC4Kna
              dgJ/S1BzHVxniYh3/rEzhXioneZ6oZgO+65WtsS42WAvh/53U/Q3chgI074Jssze
              ev09+zU8Xj0vX/7KpRKy5Vln6RGkQbKAIt7TZL5cJALswQDzcCO4WTv1X5KoG3+D
              KfTMfl9HzFsv59uHKlUqUguN5e8CLdmjgU1v2WvHBCw1PArIE8ZC0Tu2bMi5i9Vq
              GHxVn9O4Et5yPocyQtE4zOfGfqwR/yNa//Zs1b6DxQ73tq7rbBQaAzq7lxW6Ndbr
              43jjLL40ONdFxX7qW/DhT9MCAwEAAQ==
              -----END PUBLIC KEY-----
            '';
            pubkey_ed25519 = "LgJ7+/sq7t+Ym/DjJrWesIpUw1Lw7bxPi0XFHtsVWLB";
          };
        };
        wiregrill = {
          ip6.addr = w6 "3";
          aliases = [
            "echelon.w"
          ];
          wireguard.pubkey = ''
            SLdk0lph2rSFU+3dyrWDU1CT/oU+HPcOVYeGVIgDpEc=
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIn+o0uCBSot254kZKlNepVKFcwDPdr8s6+lQmYGM3Hd ";
      syncthing.id = "TT4MBZS-YNDZUYO-Y6L4GOK-5IYUCXY-2RKFOSK-5SMZYSR-5QMOXSS-6DNJIAZ";
    };

    lasspi = {
      consul = false;
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.1.89";
          ip6.addr = r6 "189";
          aliases = [
            "lasspi.r"
          ];
          tinc = {
            pubkey = ''
              -----BEGIN PUBLIC KEY-----
              MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA3zUXIiw8/9okrGaxlAR1
              JvoXNxAzLj5wwE2B0A+9ppev7Vl52HJarNoM6+0RN4aZDGMhDWg8J5ZQSdGUNm5F
              CIdxE1TwLXxzW5nd7BIb+MVsjtw0pxId7Gxq6Wgtx1QljUdsp8OVrJActqsmXYMl
              oYEWdENHRONYTCyhs+Kd18MERyxQCqOXOnD170iaFuCcHiIa2nSOtlk+aIPNIE/P
              Qsp7Q0RCRvqd5LszsI7bp3gZL9mgGquQEW+3ZxSaIYHGTdK/zI4PHYpEa7IvdJFS
              BJjJj+PbilnSxy7iL826O8ckxBqA0rNS0EynCKCI0DoVimCeklk20vLagDyXiDyC
              VW2774j1rF35eIowPTBVJNfquEptNDl9MLV3MC2P8gnCZp5x+7dEwpqsvecBQ7Z8
              +Ry9JZ/zlWi5qT86SrwKKqJqRhWHjZZSRzWdo4ypaNOy0cKHb2DcVfgn38Kf16xs
              QM11XLCRE8VLIVl5UFgrF6q/0f8JP1BG8RO90NDsLwIW/EwKiJ9OGFtayvxkmgHP
              zgmzgws8cn50762OPkp4OVzVexN77d9N8GU9QXAlsFyn2FJlO26DvFON4fHIf0bP
              6lqI1Up2jAy0eSl2txlxxKbKRlkIaebHulhxIxQ1djA+xPb/5cfasom9Qqwf6/Lc
              287nChBcbY+HlshTe0lZdrkCAwEAAQ==
              -----END PUBLIC KEY-----
            '';
            pubkey_ed25519 = "vSCHU+/BkoCo6lL5OmikALKBWgkRY8JRo4q8ZZRd5EG";
          };
        };
        wiregrill = {
          ip6.addr = w6 "189";
          aliases = [
            "lasspi.w"
          ];
          wireguard.pubkey = ''
            IIBAiG7jZEliQJJsNUQswLsB5FQFkAfq5IwyHAp71Vw=
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEjYOaTQE9OvvIaWWjO+3/uSy7rvnhnJA48rWYeB2DfB";
    };

    domsen-pixel = {
      consul = false;
      nets = {
        wiregrill = {
          ip4.addr = "10.244.1.17";
          ip6.addr = w6 "d0";
          aliases = [
            "domsen-pixel.w"
          ];
          wireguard.pubkey = "cGuBSB1DftIsanbxrSG/i4FiC+TmQrs+Z0uE6SPscHY=";
        };
      };
      external = true;
      ci = false;
    };

  };
  users = rec {
    lass = lass-yubikey;
    lass-yubikey = {
      mail = "lass@lassul.us";
      pubkey = builtins.readFile ./ssh/yubikey.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/yubikey.pgp;
    };
    lass-blue = {
      mail = "lass@blue.r";
      pubkey = builtins.readFile ./ssh/blue.rsa;
    };
    lass-green = {
      mail = "lass@green.r";
      pubkey = builtins.readFile ./ssh/green.ed25519;
    };
    lass-mors = {
      mail = "lass@mors.r";
      pubkey = builtins.readFile ./ssh/mors.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/mors.pgp;
    };
    lass-android = {
      mail = "lassulus@gmail.com";
      pubkey = builtins.readFile ./ssh/android.ed25519;
    };
    lass-tablet = {
      pubkey = builtins.readFile ./ssh/tablet.ed25519;
    };
  };
}
