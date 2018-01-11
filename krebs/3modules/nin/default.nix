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
     axon= {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.134.66";
          ip6.addr = "42:0000:0000:0000:0000:0000:0000:1379";
          aliases = [
            "axon.r"
          ];
          tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIECgKCBAEA89h5SLDQL/ENM//3SMzNkVnW4dBdg1GOXs/SdRCTcgygJC0TzsAo
          glfQhfS+OhFSC/mXAjP8DnN7Ys6zXzMfJgH7TgVRJ8tCo5ETehICA19hMjMFINLj
          KZhhthPuX7u2Jr4uDMQ0eLJnKVHF4PmHnkA+JGcOqO7VSkgcqPvqPMnJFcMkGWvH
          L3KAz1KGPHZWrAB2NBDrD/bOZj4L39nS4nJIYVOraP7ze1GTTC7s/0CnZj3qwS5j
          VdUYgAR+bdxlWm1B1PPOjkslP6UOklQQK4SjK3ceLYb2yM7BVICeznjWCbkbMACY
          PUSvdxyiD7nZcLvuM3cJ1M45zUK+tAHHDB5FFUUAZ+YY/Xml4+JOINekpQdGQqkN
          X4VsdRGKpjqi+OXNP4ktDcVkl8uALmNR6TFfAEwQJdjgcMxgJGW9PkqvPl3Mqgoh
          m89lHPpO0Cpf40o6lZRG42gH1OR7Iy1M234uA08a3eFf+IQutHaOBt/Oi0YeiaQp
          OtJHmWtpsQRz24/m+uroSUtKZ63sESli28G1jP73Qv7CiB8KvSX0Z4zKJOV/CyaT
          LLguAyeWdNLtVg4bGRd7VExoWA+Rd9YKHCiE5duhETZk0Hb9WZmgPdM7A0RBb+1H
          /F9BPKSZFl2e42VEsy8yNmBqO8lL7DVbAjLhtikTpPLcyjNeqN99a8jFX4c5nhIK
          MVsSLKsmNGQq+dylXMbErsGu3P/OuCZ4mRkC32Kp4qwJ+JMrJc8+ZbhKl6Fhwu0w
          7DwwoUaRoMqtr2AwR+X67eJsYiOVo5EkqBo6DrWIM6mO2GrWHg5LTBIShn08q/Nm
          ofPK2TmLdfqBycUR0kRCCPVi82f9aElmg3pzzPJnLAn9JLL43q6l+sefvtr9sTs3
          1co6m8k5mO8zTb8BCmX2nFMkCopuHeF1nQ33y6woq0D8WsXHfHtbPwN9eYRVrbBF
          29YBp5E+Q1pQB+0rJ4A5N1I3VUKhDGKc72pbQc8cYoAbDXA+RKYbsFOra5z585dt
          4HQXpwj3a/JGJYRT6FVbJp4p8PjwAtN9VkpXNl4//3lXQdDD6aQ6ssXaKxVAp2Xj
          FjPjx6J6ok4mRvofKNAREt4eZUdDub34bff6G0zI7Vls9t4ul0uHsJ6+ic3CG+Yl
          buLfOkDp4hVCAlMPQ2NJfWKSggoVao7OTBPTMB3NiM56YOPptfZgu2ttDRTyuQ7p
          hrOwutxoy/abH3hA8bWj1+C23vDtQ2gj0r16SWxpPdb3sselquzKp9NIvtyRVfnG
          yYZTWRHg9mahMC2P0/wWAQVjKb0LnTib4lSe21uqFkWzp+3/Uu+hiwP5xGez/NIi
          ahyL7t0D9r9y+i1RPjYWypgyR568fiGheQIDAQAB
          -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF4ubHA2pQzV4tQq9D1zRTD1xOSR6xZM3z6te+5A1ekc";
    };
    onondaga = {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.132.55";
          ip6.addr = "42:0000:0000:0000:0000:0000:0000:1357";
          aliases = [
            "onondaga.r"
            "cgit.onondaga.r"
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
      mail = "nin@axon.r";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCl4jHl2dya9Tecot7AcHuk57FiPN0lo8eDa03WmTOCCU7gEJLgpi/zwLxY/K4eXsDgOt8LJwddicgruX2WgIYD3LnwtuN40/U9QqqdBIv/5sYZTcShAK2jyPj0vQJlVUpL7DLxxRH+t4lWeRw/1qaAAVt9jEVbzT5RH233E6+SbXxfnQDhDwOXwD1qfM10BOGh63iYz8/loXG1meb+pkv3HTf5/D7x+/y1XvWRPKuJ2Ml33p2pE3cTd+Tie1O8CREr45I9JOIOKUDQk1klFL5NNXnaQ9h1FRCsnQuoGztoBq8ed6XXL/b8mQ0lqJMxHIoCuDN/HBZYJ0z+1nh8X6XH nin@axon";
    };
    nin_h = {
      mail = "nin@hiawatha.r";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDicZLUPEVNX7SgqYWcjPo0UESRizEfIvVVbiwa1aApA8x25u/5R3sevcgbIpLHYKDMl5tebny9inr6G2zqB6oq/pocQjHxrPnuLzqjvqeSpbjQjlNWJ9GaHT5koTXZHdkEXGL0vfv1SRDNWUiK0rNymr3GXab4DyrnRnuNl/G1UtLf4Zka94YUD0SSPdS9y6knnRrUWKjGMFBZEbNSgHqMGATPQP9VDwKHIO2OWGfiBAJ4nj/MWj+BxHDleCMY9zbym8yY7p/0PLaUe9eIyLC8MftJ5suuMmASlj+UGWgnqUxWxsMHax9y7CTAc23r1NNCXN5LC6/facGt0rEQrdrTizBgOA1FSHAPCl5f0DBEgWBrRuygEcAueuGWvI8/uvtvQQZLhosDbXEfs/3vm2xoYBe7wH4NZHm+d2LqgIcPXehH9hVQsl6pczngTCJt0Q/6tIMffjhDHeYf6xbe/n3AqFT0PylUSvOw/H5iHws3R6rxtgnOio7yTJ4sq0NMzXCtBY6LYPGnkwf0oKsgB8KavZVnxzF8B1TD4nNi0a7ma7bd1LMzI/oGE6i8kDMROgisIECOcoe8YYJZXIne/wimhhRKZAsd+VrKUo4SzNIavCruCodGAVh2vfrqRJD+HD/aWH7Vr1fCEexquaxeKpRtKGIPW9LRCcEsTilqpZdAiw== nin@hiawatha";
    };
  };
}
