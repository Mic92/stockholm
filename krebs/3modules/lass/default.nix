{ lib, ... }:

with import ../../4lib { inherit lib; };

let
  testHosts = lib.genAttrs [
    "test-arch"
    "test-centos6"
    "test-centos7"
  ] (name: {
    inherit name;
    nets = {
      retiolum = {
        addrs4 = ["10.243.111.111"];
        addrs6 = ["42:0:0:0:0:0:0:7357"];
        aliases = [
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
  hosts = addNames {
    echelon = {
      cores = 4;
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

  } // testHosts;
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
