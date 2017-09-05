{ config, ... }:

with import <stockholm/lib>;

{
  hosts = mapAttrs (_: recursiveUpdate {
    owner = config.krebs.users.nin;
    ci = true;
  }) {
    hiawatha = {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.132.96";
          ip6.addr = "42:0000:0000:0000:0000:0000:0000:2342";
          aliases = [
            "hiawatha.retiolum"
            "hiawatha.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAucIe5yLzKJ8F982XRpZT6CvyXuPrtnNTmw/E/T6Oyq88m/OVHh6o
            Viho1XAlJZZwqNniItD0AQB98uFB3+3yA7FepnwwC+PEceIfBG4bTDNyYD3ZCsAB
            iWpmRar9SQ7LFnoZ6X2lYaJkUD9afmvXqJJLR5MClnRQo5OSqXaFdp7ryWinHP7E
            UkPSNByu4LbQ9CnBEW8mmCVZSBLb8ezxg3HpJSigmUcJgiDBJ6aj22BsZ5L+j1Sr
            lvUuaCr8WOS41AYsD5dbTYk7EG42tU5utrOS6z5yHmhbA5r8Ro2OFi/R3Td68BIJ
            yw/m8sfItBCvjJSMEpKHEDfGMBCfQKltCwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFizK5kauDlnjm/IzyzLi+W4hLKqjSWMkfuxzLwg6egx";
    };
    onondaga = {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.132.55";
          ip6.addr = "42:0000:0000:0000:0000:0000:0000:1357";
          aliases = [
            "onondaga.retiolum"
            "onondaga.r"
            "cgit.onondaga.r"
            "cgit.onondaga.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAqj6NPhRVsr8abz9FFx9+ld3amfxN7SRNccbksUOqkufGS0vaupFR
            OWsgj4Qmt3lQ82YVt5yjx0FZHkAsenCEKM3kYoIb4nipT0e1MWkQ7plVveMfGkiu
            htaJ1aCbI2Adxfmk4YbyAr8k3G+Zl9t7gTikBRh7cf5PMiu2JhGUZHzx9urR0ieH
            xyashZFjl4TtIy4q6QTiyST9kfzteh8k7CJ72zfYkdHl9dPlr5Nk22zH9xPkyzmO
            kCNeknuDqKeTT9erNtRLk6pjEcyutt0y2/Uq6iZ38z5qq9k4JzcMuQ3YPpNy8bxn
            hVuk2qBu6kBTUW3iLchoh0d4cfFLWLx1SQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGmQk7AXsYLzjUrOjsuhZ3+gT7FjhPtjwxv5XnuU8GJO";
    };

  };
  users = {
    nin = {
      mail = "nin@hiawatha.retiolum";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDicZLUPEVNX7SgqYWcjPo0UESRizEfIvVVbiwa1aApA8x25u/5R3sevcgbIpLHYKDMl5tebny9inr6G2zqB6oq/pocQjHxrPnuLzqjvqeSpbjQjlNWJ9GaHT5koTXZHdkEXGL0vfv1SRDNWUiK0rNymr3GXab4DyrnRnuNl/G1UtLf4Zka94YUD0SSPdS9y6knnRrUWKjGMFBZEbNSgHqMGATPQP9VDwKHIO2OWGfiBAJ4nj/MWj+BxHDleCMY9zbym8yY7p/0PLaUe9eIyLC8MftJ5suuMmASlj+UGWgnqUxWxsMHax9y7CTAc23r1NNCXN5LC6/facGt0rEQrdrTizBgOA1FSHAPCl5f0DBEgWBrRuygEcAueuGWvI8/uvtvQQZLhosDbXEfs/3vm2xoYBe7wH4NZHm+d2LqgIcPXehH9hVQsl6pczngTCJt0Q/6tIMffjhDHeYf6xbe/n3AqFT0PylUSvOw/H5iHws3R6rxtgnOio7yTJ4sq0NMzXCtBY6LYPGnkwf0oKsgB8KavZVnxzF8B1TD4nNi0a7ma7bd1LMzI/oGE6i8kDMROgisIECOcoe8YYJZXIne/wimhhRKZAsd+VrKUo4SzNIavCruCodGAVh2vfrqRJD+HD/aWH7Vr1fCEexquaxeKpRtKGIPW9LRCcEsTilqpZdAiw== nin@hiawatha";
    };
  };
}
