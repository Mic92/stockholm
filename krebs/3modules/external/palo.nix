with import <stockholm/lib>;
{ config, ... }: let

  hostDefaults = hostName: host: flip recursiveUpdate host ({
    ci = false;
    external = true;
    monitoring = false;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum.ip6.addr =
      (krebs.genipv6 "retiolum" "external" { inherit hostName; }).address;
  } // optionalAttrs (host.nets?wiregrill) {
    nets.wiregrill.ip6.addr =
      (krebs.genipv6 "wiregrill" "external" { inherit hostName; }).address;
  });
  ssh-for = name: builtins.readFile (./ssh + "/${name}.pub");
  tinc-for = name: builtins.readFile (./tinc + "/${name}.pub");

in {
  hosts = mapAttrs hostDefaults {
    pepe = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.1";
          tinc.port = 720;
          aliases = [ "pepe.r" ];
          tinc.pubkey = tinc-for "palo";
        };
      };
    };
    kruck = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.3";
          tinc.port = 720;
          aliases = [
            "kruck.r"
            "video.kruck.r"
          ];
          tinc.pubkey = tinc-for "palo";
        };
      };
    };
    schasch = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.2";
          tinc.port = 720;
          aliases = [ "schasch.r" ];
          tinc.pubkey = tinc-for "palo";
        };
      };
    };
    workhorse = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.5";
          tinc.port = 720;
          aliases = [ "workhorse.r" ];
          tinc.pubkey = tinc-for "palo";
        };
      };
    };
    workout = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.23.4";
          tinc.port = 720;
          aliases = [ "workout.r" ];
          tinc.pubkey = tinc-for "palo";
        };
      };
    };
  };
  users = {
    palo = {
    };
    palo-pepe = {
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCe3j1dk8o7e0cFn+RepOjdeYcS0YcG0d6NnsMoq8pjqzLRuvOVN4CmsuRo4UJtWOUVv8kIWLcegks2RXbKOSTFAnvEIpPlSmL3BgvFGmUiOmPw1w93yxdVnfd9kJ6SBNuS+kFspKPvOEV9yVpjTywKZ0qKWjRd89Vz2+8aFJ/R3hyE+YqpzlzYbrucEIXbEWESqnzrx61NrWwd1ueHj0RYMDBjIJjVfIIK3W32vQ//pYJio1di0PpOPK2Ya0yugSixymuQBvzCgaedVeLdJ0k1d7d5iVb4LjCR8zg0Jnf1RLfpqRrFYwMJpwuhtIEevfNrhzqZA58QgHO6iOg50FeaD5k1rlWfoOvX2rcV5iqCC9jClIfMKzePdm+MeVorBXp+bflOhyPJG+Qrz6NTE9Ohe5A71Z0bBa96MEoW1hyrLCn0+z+Cx5kt7n2QzAAa/VPNjRDZeHbsu26MrvViEoh+FcPqx4DUIUaDRs/TNIvMGAl14E6gur68AI01a0PZJ/gnqZeOJKuyz4pKT0nIcg0EvkjXM2uEJ9m8h4IBAqTUYMZYm7iJBfpbwT6ePxFfah4q960orBUwW87CyPu/wDGmDblQ7dpkuw+skpzNgOGzerDyGAGKOsjGfYZpqsv9303S2f7884NAIK1ohgpMELytEYpY4YNi2KQYfHhgoQRJ5w== palo@pepe";
    };
  };
}

