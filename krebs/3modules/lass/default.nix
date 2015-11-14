{ lib, ... }:

with lib;

{
  hosts = addNames {
    echelon = {
      cores = 2;
      dc = "lass"; #dc = "cac";
      nets = rec {
        internet = {
          addrs4 = ["167.88.34.158"];
          aliases = [
            "echelon.internet"
          ];
        };
        retiolum = {
          via = internet;
          addrs4 = ["10.243.206.103"];
          addrs6 = ["42:941e:2816:35f4:5c5e:206b:3f0b:f763"];
          aliases = [
            "echelon.retiolum"
            "cgit.echelon.retiolum"
            "go.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAuscWOYdHu0bpWacvwTNd6bcmrAQ0YFxJWHZF8kPZr+bMKIhnXLkJ
            oJheENIM6CA9lQQQFUxh2P2pxZavW5rgVlJxIKeiB+MB4v6ZO60LmZgpCsWGD/dX
            MipM2tLtQxYhvLJIJxEBWn3rxIgeEnCtZsH1KLWyLczb+QpvTjMJ4TNh1nEBPE/f
            4LUH1JHaGhcaHl2dLemR9wnnDIjmSj0ENJp2al+hWnIggcA/Zp0e4b86Oqbbs5wA
            n++n5j971cTrBdA89nJDYOEtepisglScVRbgLqJG81lDA+n24RWFynn+U3oD/L8p
            do+kxlwZUEDRbPU4AO5L+UeIbimsuIfXiQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL21QDOEFdODFh6WAfNp6odrXo15pEsDQuGJfMu/cKzK";
    };
    prism = {
      cores = 4;
      dc = "lass"; #dc = "cac";
      nets = rec {
        internet = {
          addrs4 = ["213.239.205.240"];
          aliases = [
            "prism.internet"
          ];
        };
        retiolum = {
          via = internet;
          addrs4 = ["10.243.0.103"];
          addrs6 = ["42:0000:0000:0000:0000:0000:0000:15ab"];
          aliases = [
            "prism.retiolum"
            "cgit.prism.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAvzhoBsxUaEwm7ctiw3xvLFP2RoVaiHnF+Sm4J8E4DOerPToXxlyl
            kxvMPaRnhtiO6MK0Vv2+VswKIeRkMm5YuD5MG7wni4vUKcRx9cCgKji/s0vGqLhl
            JKK9i23q7epvQ32Is/e3P+fQ5KM50EO+TWACNaroCNoyJvZ/G8BWXw6WnIOsuX0I
            AoPW2ol8/sdZxeK4hCe/aQz6y0AEvigpvPkHx+TE5fkBeIeqhiKTIWpEqjU4wXx5
            jP2izYuaIsHAihU8mm03xRxT4+4IHYt6ddrhNeBuJBsATLkDgULdQyOoEzmXCm2j
            anGRBZoYVazxn7d8mKBdE09ZNc1ijULZgwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_rsa>;
      ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChm4sqQ2bUZj+2YnTf6G5HHRTpSe1jTUhJRnwcYPYZKF+CBqBncipRpuGlGXEsptNa+7ZMcQC0ySsz5SUOMt3Ih+NehVe/qt3VtRz0l0MgOWmH2qBwKK9Y4IuxrJQzUmP4UGlOGlFj9DORssSMOyFIG4eZ9k2qMn3xal0NVRfGTShKlouWsiUILZ8I+sDNE00z8DAYesgc1yazvRnjzvLkRxdNdpYiAFBbmXMpPKK95McRJaWsuNSeal9kd5p5PagWcgN4DZ6+ebzz3NKnmzk4j+vuHX0U9lTXBqKMlzzmM2YNLRtDPfrtJNyHqLpZUpFhJKqZCD+4/0zdrzRfC7Th+5czzUCSvHiKPVsqw5eOdiQX6EyzNAF5zpkpRp//QdUNNXC5/Ku6GKCO491+TuA8VCha0fOwBONccTLUI/hGNmCh88mLbukVoeGJrbYNCOA/6kEz7ZLEveU4i+TT7okhDElMsNk+AWCZ8/NdJQNX3/K6+JJ9qAn+/yC8LdjgYYJ2oU/aw5/HyOgiQ0z4n9UfQ7j+nHysY9CQb1b3guX7yjJoc3KpNXCXEztuIRHjFD1EP8NRTSmGjsa/VjLmTLSsqjD+7IE5mT0tO5RJvmagDgdJSr/iR5D9zjW7hx7ttvektrlp9g0v3CiCFVaW4l95hGYT0HaNBLJ5R0YHm0lD+Q==";
    };
    fastpoke = {
      dc = "lass";
      nets = rec {
        internet = {
          addrs4 = ["193.22.164.36"];
          aliases = [
            "fastpoke.internet"
          ];
        };
        retiolum = {
          via = internet;
          addrs4 = ["10.243.253.152"];
          addrs6 = ["42:422a:194f:ff3b:e196:2f82:5cf5:bc00"];
          aliases = [
            "fastpoke.retiolum"
            "cgit.fastpoke.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAs4p5xsQYx06v+OkUbc09K6voFAbkvO66QdFoM71E10XyCeLP6iuq
            DaIOFN4GrPR36pgyjqtJ+62G9uR+WsB/y14eio1p1ivDWgcpt5soOZAH5zVRRD9O
            FBDlgVNwIJ6stMHy6OenEKWsfEiZRN3XstnqAqyykzjddglth1tJntn6kbZehzNQ
            ezfIyN4XgaX2fhSu+UnAyLcV8wWnF9cMABjz7eKcSmRJgtG4ZiuDkbgiiEew7+pB
            EPqOVQ80lJvzQKgO4PmVoAjD9A+AHnmLJNPDQQi8nIVilGCT60IX+XT1rt85Zpdy
            rEaeriw/qsVJnberAhDAdQYYuM1ai2H5swIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    cloudkrebs = {
      cores = 1;
      dc = "lass"; #dc = "cac";
      nets = rec {
        internet = {
          addrs4 = ["104.167.113.104"];
          aliases = [
            "cloudkrebs.internet"
          ];
        };
        retiolum = {
          via = internet;
          addrs4 = ["10.243.206.102"];
          addrs6 = ["42:941e:2816:35f4:5c5e:206b:3f0b:f762"];
          aliases = [
            "cloudkrebs.retiolum"
            "cgit.cloudkrebs.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAttUygCu7G6lIA9y+9rfTpLKIy2UgNDglUVoKZYLs8JPjtAtQVbtA
            OcWwwPc8ijLQvwJWa8e/shqSzSIrtOe+HJbRGdXLdBLtOuLKpz+ZFHcS+95RS5aF
            QTehg+QY7pvhbrrwKX936tkMR568suTQG6C8qNC/5jWYO/wIxFMhnQ2iRRKQOq1v
            3aGGPC16KeXKVioY9KoV98S3n1rZW1JK07CIsZU4qb5txtLlW6FplJ7UmhVku1WC
            sgOOj9yi6Zk1t8R2Pwv9gxa3Hc270voj5U+I2hgLV/LjheE8yhQgYHEA4vXerPdO
            TGSATlSmMtE2NYGrKsLM7pKn286aSpXinwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN7oYx7Lbkc0wPYNp92LQF93DCtxsGzOkVD91FJQzVZl";
    };
    uriel = {
      cores = 1;
      dc = "lass";
      nets = {
        gg23 = {
          addrs4 = ["10.23.1.12"];
          aliases = ["uriel.gg23"];
        };
        retiolum = {
          addrs4 = ["10.243.81.176"];
          addrs6 = ["42:dc25:60cf:94ef:759b:d2b6:98a9:2e56"];
          aliases = [
            "uriel.retiolum"
            "cgit.uriel.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAzw0pvoEmqeqiZrzSOPH0IT99gr1rrvMZbvabXoU4MAiVgGoGrkmR
            duJkk8Fj12ftMc+Of1gnwDkFhRcfAKOeH1RSc4CTircWVq99WyecTwEZoaR/goQb
            MND022kIBoG6NQNxv1Y5I1B/h7hfloMFEPym9oFtOAXoGhBY2vVl4g64NNz+RLME
            m1RipLXKANAh6LRNPGPQCUYX4TVY2ZJVxM3CM1XdomUAdOYXJmWFyUg9NcIKaacx
            uRrmuy7J9yFBcihZX5Y7NV361kINrpRmZYxJRf9cr0hb5EkJJ7bMIKQMEFQ5RnYo
            u7MPGKD7aNHa6hLLCeIfJ5u0igVmSLh3pwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBryIo/Waw8SWvlQ0+5I+Bd/dJgcMd6iPXtELS6gQXoc";
      secure = true;
    };
    mors = {
      cores = 2;
      dc = "lass";
      nets = {
        gg23 = {
          addrs4 = ["10.23.1.11"];
          aliases = ["mors.gg23"];
        };
        retiolum = {
          addrs4 = ["10.243.0.2"];
          addrs6 = ["42:0:0:0:0:0:0:dea7"];
          aliases = [
            "mors.retiolum"
            "cgit.mors.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAsj1PCibKOfF68gmFQ+wwyfhUWpqKqpznrJX1dZ+daae7l7nBHvsE
            H0QwkiMmk3aZy1beq3quM6gX13aT+/wMfWnLyuvT11T5C9JEf/IS91STpM2BRN+R
            +P/DhbuDcW4UsdEe6uwQDGEJbXRN5ZA7GI0bmcYcwHJ9SQmW5v7P9Z3oZ+09hMD+
            1cZ3HkPN7weSdMLMPpUpmzCsI92cXGW0xRC4iBEt1ZeBwjkLCRsBFBGcUMuKWwVa
            9sovca0q3DUar+kikEKVrVy26rZUlGuBLobMetDGioSawWkRSxVlfZvTHjAK5JzU
            O6y6hj0yQ1sp6W2JjU8ntDHf63aM71dB9QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINAMPlIG+6u75GJ3kvsPF6OoIZsU+u8ZQ+rdviv5fNMD";
    };
    schnabel-ap = {
      nets = {
        gg23 = {
          addrs4 = ["10.23.1.20"];
          aliases = ["schnabel-ap.gg23"];
        };
      };
    };
    Reichsfunk-ap = {
      nets = {
        gg23 = {
          addrs4 = ["10.23.1.10"];
          aliases = ["Reichsfunk-ap.gg23"];
        };
      };
    };

  };
  users = addNames {
    lass = {
      pubkey = readFile ../../Zpubkeys/lass.ssh.pub;
      mail = "lass@mors.retiolum";
    };
    uriel = {
      pubkey = readFile ../../Zpubkeys/uriel.ssh.pub;
      mail = "lass@uriel.retiolum";
    };
  };
}
