{ config, lib, ... }: {
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
        "krebs.ni.r"
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
        (lib.krebs.genipv6 "wiregrill" "tv" 0).subnetCIDR
      ];
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILGDdcKwFm6udU0/x6XGGb87k9py0VlrxF54HeYu9Izb";
}
