{ config, ... }:

with config.krebs.lib;
let
  testHosts = genAttrs [
    "test-arch"
    "test-centos6"
    "test-centos7"
    "test-all-krebs-modules"
  ] (name: {
    owner = config.krebs.users.shared;
    inherit name;
    cores = 1;
    nets = {
      retiolum = {
        ip4.addr = "10.243.111.111";
        ip6.addr = "42:0:0:0:0:0:0:7357";
        aliases = [
          "test.r"
          "test.retiolum"
        ];
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAy41YKF/wpHLnN370MSdnAo63QUW30aw+6O79cnaJyxoL6ZQkk4Nd
          mrX2tBIfb2hhhgm4Jecy33WVymoEL7EiRZ6gshJaYwte51Jnrac6IFQyiRGMqHY5
          TG/6IzzTOkeQrT1fw3Yfh0NRfqLBZLr0nAFoqgzIVRxvy+QO1gCU2UDKkQ/y5df1
          K+YsMipxU08dsOkPkmLdC/+vDaZiEdYljIS3Omd+ED5JmLM3MSs/ZPQ8xjkjEAy8
          QqD9/67bDoeXyg1ZxED2n0+aRKtU/CK/66Li//yev6yv38OQSEM4t/V0dr9sjLcY
          VIdkxKf96F9r3vcDf/9xw2HrqVoy+D5XYQIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
      };
    };
  });
in {
  hosts = {
    wolf = {
      owner = config.krebs.users.shared;
      nets = {
        shack = {
          ip4.addr =  "10.42.2.150" ;
          aliases = [
            "wolf.shack"
            "graphite.shack"
            "acng.shack"
            "drivedroid.shack"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.77.1";
          ip6.addr = "42:0:0:0:0:0:77:1";
          aliases = [
            "wolf.retiolum"
            "cgit.wolf.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAzpXyEATt8+ElxPq650/fkboEC9RvTWqN6UIAl/R4Zu+uDhAZ2ekb
            HBjoSbRxu/0w2I37nwWUhEOemxGm4PXCgWrtO0jeRF4nVNYu3ZBppA3vuVALUWq7
            apxRUEL9FdsWQlXGo4PVd20dGaDTi8M/Ggo755MStVTY0rRLluxyPq6VAa015sNg
            4NOFuWm0NDn4e+qrahTCTiSjbCU8rWixm0GktV40kdg0QAiFbEcRhuXF1s9/yojk
            7JT/nFg6LELjWUSSNZnioj5oSfVbThDRelIld9VaAKBAZZ5/zy6T2XSeDfoepytH
            8aw6itEuTCy1M1DTiTG+12SPPw+ubG+NqQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKYMXMWZIK0jjnZDM9INiYAKcwjXs2241vew54K8veCR";
    };
  } // testHosts;
  users = {
    shared = {
      mail = "spam@krebsco.de";
      pubkey = "lol"; # TODO krebs.users.shared.pubkey should be unnecessary
    };
  };
}
