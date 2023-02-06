{ config, krebs, r6, w6, ... }:
rec {
  extraZones = {
    "krebsco.de" = ''
      cache     60 IN A ${nets.internet.ip4.addr}
      p         60 IN A ${nets.internet.ip4.addr}
      c         60 IN A ${nets.internet.ip4.addr}
      paste     60 IN A ${nets.internet.ip4.addr}
      prism     60 IN A ${nets.internet.ip4.addr}
      social    60 IN A ${nets.internet.ip4.addr}
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
      mail                60 IN AAAA   ${config.krebs.hosts.prism.nets.internet.ip6.addr}
      flix                60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
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
        "paste.r"
        "c.r"
        "p.r"
        "search.r"
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
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAsANFdMi825qWQXQbWLYuNZ6/fARt3lnh1KStQHQQMD";
  syncthing.id = "QITFKYQ-VEPIPL2-AZIXHMD-BBT62ML-YHSB35A-BSUIBXS-QYMPFHW-M7XN2QU";
}
