{ config, ... }:

with import <stockholm/lib>;

{
  hosts = mapAttrs (_: setAttr "owner" config.krebs.users.mv) {
    stro = {
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.111.111";
          ip6.addr = "42:0:0:0:0:0:111:111";
          aliases = [
            "stro.r"
            "cgit.stro.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA0vIzLyoetOyi3R7qOh3gjSvUVjPEdqCvd0NEevDCIhhFy0nIbZ/b
            vnuk3EUeTb6e384J8fKB4agig0JeR3JjtDvtjy5g9Cdy2nrU71w8wqU0etmv2PTb
            FjbCFfeBXn0N3U7gXwjZGCvjAXa1a4jGb4R2iYBYGG3aY4reCN8B8Ah81h+S0oLg
            ZJJfaBmWM5vNRFEI5X4CLaVnwtsoZuXIjYStgNn/9Mg/Y6NQS0H0H+HFeyhigAqG
            oYGqNar/2QqPU176V/FwrD30F3qJV1uyzuPta7hmdfOxqYjZ/jqdPSRYtlunYYcq
            XbH5oYmzO9NEeVWzjdac/DiV2OP8HufoYwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM+7Qa51l0NSkBiaK2s8vQEoeObV3UPZyEzMxfUK/ZAO root@stro";
    };
  };
  users = {
    mv = {
      mail = "mv@stro.r";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDfMqkfXsGRaXJ86Pi5svAx4508ij5kc4cMLGwr1CLvFI5G7EHggiHMZYooibmkZimBF1PvLM1lOdoptJ4nSmc3UGuQaeV9BpZ1dNXexc8wOmVPKzAHYZG/2upcV/xVZQ9lk3UOmDym6fDUXThMx4nXdhOjScgWpKp7+0N3JRCf2UHusZjWFGlhE9l4irLFHCwlZeBQ16DNF4fc03vsfZBB1ZrGGZlaVpkcY+FTC3sm8R0iF5QGaq8PgltJoCNnp3L1g3Yn7Elva7kCHjZfJC1pu5icV8vZMNptPn1b10gPsNwb302FCjvZohzRcMo39L2gwdNWQmflYfYk+NPY9EgqkLtSvZJywYu8oTVLeYBAp0ZGzJR4+uIH9at/WQF499HFMxpF4uwYiQweUcPiHrrOqI5zLQoOvqh9Jv0UMsnFynNrszbCTgwzeW8bcvv8ILcjE9of8GXRCrlIMvt7Z9q8xrb5j1RhKscvusyyNOAL+HMZl6jgSxUBDtzRqPZ62QHJsBEBdRXdJRQLGeHNW9kGPrh/tiKGucuT3/HZC+2Rcemxt3RVT60+lHkghrMLi0/VOWBUKL9J94UK5xIE4Gb3RTW9DcNK53U4ql+N4ORSSEuhk3Rqzx3Bzv7AXpLKQCFKdB7tjxzGN7sCQM3PBUUo6Tk0VG2cIKOjzTRnDJlb7Q== mv@stro";
    };
  };
}
