{ config, ... }:

with import <stockholm/lib>;
let
  testHosts = genAttrs [
    "test-arch"
    "test-centos6"
    "test-centos7"
    "test-all-krebs-modules"
  ] (name: {
    owner = config.krebs.users.krebs;
    inherit name;
    cores = 1;
    nets = {
      retiolum = {
        ip4.addr = "10.243.73.57";
        ip6.addr = "42:0:0:0:0:0:0:7357";
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
    puyak = {
      owner = config.krebs.users.krebs;
      nets = {
        retiolum = {
          ip4.addr = "10.243.77.2";
          ip6.addr = "42:0:0:0:0:0:77:2";
          aliases = [
            "puyak.r"
            "build.puyak.r"
            "cgit.puyak.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAwwDvaVKSJmAi1fpbsmjLz1DQVTgqnx56GkHKbz5sHwAfPVQej955
            SwotAPBrOT5P3pZ52Pu326SR5nj9XWfN6GD0CkcDQddtRG5OOtUWlvkYzZraNh33
            p9l8TBgHJKogGe6umbs+4v7pWfbS0k708L2ttwY0ceju6RL6UqShIYB6qhDzwalU
            p8s7pypl7BwrsTwYkUGleIptiN78cYv/NHvXhvXBuVGz4J0tCH4GMvdTHCah1l1r
            zwEpKlAq0FD6bgYTJL94Tvxe2xzyr8c+xn1+XbJtMudGmrRjIHS6YupzO/Y2MO7w
            UkbMKDhYVhSPFEyk6PMm0SU9uAh4I1+8BQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPpVwKv9mQGfcn5oFwuitq+b6Dz4jBG9sGhVoCYFw5RY";
    };
    wolf = {
      owner = config.krebs.users.krebs;
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
            "wolf.r"
            "build.wolf.r"
            "cgit.wolf.r"
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
    krebs = {
      pubkey = "lol"; # TODO krebs.users.krebs.pubkey should be unnecessary
    };
    puyak-repo-sync = {
      name = "puyak-repo-sync";
      mail = "spam@krebsco.de";
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIILKVAxlz6L4yLL/4+MFk0YyzQSK+XI4ayxNQfLKepMj";
    };
    wolf-repo-sync = {
      name = "wolf-repo-sync";
      mail = "spam@krebsco.de";
      pubkey = ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCwuAZB3wtAvBJFYh+gWdyGaZU4mtqM2dFXmh2rORlbXeh02msu1uv07ck1VKkQ4LgvCBcBsAOeVa1NTz99eLqutwgcqMCytvRNUCibcoEWwHObsK53KhDJj+zotwlFhnPPeK9+EpOP4ngh/tprJikttos5BwBwe2K+lfiid3fmVPZcTTYa77nCwijimMvWEx6CEjq1wiXMUc4+qcEn8Swbwomz/EEQdNE2hgoC3iMW9RqduTFdIJWnjVi0KaxenX9CvQRGbVK5SSu2gwzN59D/okQOCP6+p1gL5r3QRHSLSSRiEHctVQTkpKOifrtLZGSr5zArEmLd/cOVyssHQPCX repo-sync@wolf'';
    };
  };
}
