{ config, ... }: let
  lib = import ../../lib;
in {

  users.jan = {
    mail = "jan.heidbrink@posteo.de";
  };

  hosts.toastbrot = {
     owner = config.krebs.users.jan;
     nets = {
      retiolum = {
        ip4.addr = "10.243.117.12";
        aliases = [
          "toastbrot.r"
        ];
        tinc.pubkey = ''
          -----BEGIN PUBLIC KEY-----
          MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA12VLPJMhGSh5fQgrB6bP
          2H1eew0/7t1xr3oJ3uaTZd7UIvPQ/aA5pB9xL5s+BIBvRa5P3QFWUAVhqchsPiRc
          yC4awLvo6zrUZB3pJBFiUuThx1xzmazTbRNyJ0E3Dwi2VSp3dAi5xEwHSVDSElGj
          DyRrdwyLe9lKghGHgNhB01QAt1/AO3A/TBs2RS/E0kuPhVQzpo5Ae5I530Cr0pf3
          r/de1TdArIcOfnTvW7WNrdBhwLq14cfdXkZwJ2bBE9Q22FAJp5k21PW5dQ41oDuT
          PYHZIH555sxifMThrUpuNHIrDtIQk6D+Km90WNf/lBGwZqQr/B5G6zSNX7d/0JbY
          Hi8Ltq++Sf0XgWNir9+evGNLCBqAXdvQFrj2l7BuNywE0L2nZThnxjTxP6QLFnqO
          IXY97x3p7AYcfmVFutfYqYM1HdyyehF711hhm30fdcXHsJ+GpQgGrj67+++N7g7g
          fjWBGNI9EL9CyTZ/N9U3TGeoxooc1BSaAiHmaPoYaAeI0Y/W6bNrixpL3aI5X8MH
          Flen2y2XEk2n+pXozPDbLAT+MZ3sWwODDYRc8zGbV2RlMvL94LHh95/JC0itdXa3
          uNRDtSnfbNe4eHw9/HMDkclhywuE+hbyq+JNNodqLwG/o1/r3GI+ggOyCdZHjF4B
          4R8QXUJiqUdcbR3WQDR5i10CAwEAAQ==
          -----END PUBLIC KEY-----
        '';
      };
    };
  };

  hosts.petrosilia = {
    owner = config.krebs.users.jan;
    nets = {
      retiolum = {
        ip4.addr = "10.243.143.11";
        aliases = [
          "petrosilia.r"
        ];
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIICCgKCAgEAxDumQ/06Yd3AQPSlHH9/kNngbc/tq5yBuT0ymbQGMHLL9X3pCz/f
          y9GZVpQtaKm7EZ0Kj8ieaPOyG7BItH0AvTdSJV7rn4WKuKfe5E5S4E8YqsZfSu4N
          IdEKVIisyBNCklXaDn6A7nxeUauwHQHuj0wOAnYKfaU+2haL+JzcFtQ1RpxDBsy1
          FbcEXO5NOhsXK4mHjtRrK1GamnCo5gvJU3w1NrfLRXteOOBsR49HhTIWvi8L4tSf
          fd/mFwWayB7D0feLhWBpMPQTa5TeeQPxhgJrlIwXJiONG8GWFWNCHEjbQaCuJJWn
          e37n9xCpdH867P921Ei+gyKZi9t6d+U4blrCpQzIe95t8Uv0i2c+YNt9NQL5Z119
          jt/Xhm7ccT9FeOuYsbjcO6g0BJumILEjD309vfQfWNims++vMd53q3dzxp4Kau+f
          vdMyrzWiIytM+/iQmneG8XLv0b7I6FUPEahpCncZ14NqBDaKclwoJ/HfB+WZi6JV
          yBVJHm9vogfzD1sLmDctHps3uJAeZHzszws8LMKdd5JxxQzVBRcrD1LKHYmmUYTU
          5gyDxnFn8ZoZ3GFVH+5v2PJgZY++/6zdDxQ9flrdt2zRaoAq2Zayn7R8sQ/ZjMXK
          eR8aXgHzEL/n/9BMKs+jLu3j8xaiJX8ctnRvwSnOFjU9wQvJ7QNQHk0CAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "Rs5jdJk/YF4aXohp3isau4LHinD4VWlvSa9CcgznR+A";
      };
    };
  };

  hosts.grill = {
    owner = config.krebs.users.jan;
    nets.retiolum = {
      aliases = [ "grill.r" ];
      ip6.addr = (lib.krebs.genipv6 "retiolum" "jan" { hostName = "grill"; }).address;
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEAs4P6CfRcwFGCqkfv1tyTbbk2eHh08kEqxPNQ655sMKWxMhgRnRII
        1ooJW+q3zOm0P4IySvQkqPCXiynPBKG+W8vz6as4/TjMgqz45zTSZaoGsUjPS7Yg
        L9qN6bLNJUhjPtyBBIX5l+WSii2RkbtcFTewY9HITPgOvu5rSiYgdz1X86BDTy0w
        E6g13jwjI0D29jFAXIIfSwfvqikHmicr++3R4URPTiY7Vcg3UtIYGaKEFTPid0Da
        bd47ZNWI99CI5Znzd4aJSD+0lfD6+EZb4nQ2o/VZ5RRUid9qWKHu5pbXvPrwE5uC
        TWtsP1nla+zx1nDD2UHt0bJzdfz4sEFrmLHBqsdvfgAlVvVr65ZMIOO5X0fevHi4
        s3jqYPMLksimuQjHCXYcgxfBYkVPuVWqDivOV8Z60UhAop5xK9i+FV4kyTgL+qmH
        79VAy8+2Wrzda/MBVFF+0XAryBtqFgk5JtmfRKJ5rcXFy9hnugmfulOC0+XFPFbN
        cNLbPR/dwON6YIg90z0wwJfPoWwzj3jKwT7YZ/pYSEl0JDgkpTzCxiBbqpJ/r8CZ
        2avRws5YMVnLcuY1IFlNLJdUZdz+41zmPizIP0dAdrwDH56AJkTukESf1Ir6G2NT
        isn3pijKy4Y/EbWnJiQpEKDfNh8JW1Ryw1zvNYKYR3OAImp3DgsWmeECAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "cqfMY/8kqtuM5wIzYMNfFIc47Jx1nnfV0//SMpsO61G";
    };
  };
}
