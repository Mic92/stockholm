{ config, ... }:

with import <stockholm/lib>;

{
  hosts = mapAttrs (_: setAttr "owner" config.krebs.users.makefu) {
    drop = rec {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.177.9";
          ip6.addr = "42:f63:ddf8:7520:cfec:9b61:d807:1dce";
          aliases = [
            "drop.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA1QxukdeDqI47nm7/gd5Y9dZZbJULA02ak0A2cB4lmysJjgMFAfbl
            6qpH7HCZk6s+4eI7H+UHUF177W7Z1qq3bqGLmlgdMMAzuDNz9UvNLhrthZMp3tCI
            GIFD28O1bKgDAYgsF/X21CRqEvgk3vRDp9yqIVIzQDmerOrZUx62Rx9Fssl/7ooW
            0319fxcTw6GZEp7RXNzgIobnWPydakh+/I0inP0rC6It/vM5Hi2bV71QPZUyJ78C
            Szh4S8TznW7yMzTQaOENeaUKfqEyN+CW2OomVdWIBOvTJVpvfAut/kg1dyUGgHlT
            F8OlAoNAyxCSxqbM0fY0wtqKD7FaYY9cbQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    studio = rec {
      cores = 4;
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIqBR5gjJkR1TEIs2yx6JRoIOA7+/LJA6kjju8yCauFa studio";
      nets = {
        retiolum = {
          ip4.addr = "10.243.227.163";
          ip6.addr  = "42:e23f:ae0e:ea25:72ff:4ab8:9bd9:38a6";
          aliases = [
            "studio.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAwAdSac8Oy5tPu7ejwojY5YqaNOfd7i0NToE+oaRJ1yxzmUpj8Fti
            cGpcgBYhFXMVYoYfzLdkAlSYjWKAoShCq/ZEfIM67okXegXvL68zGksfXrmpdUuk
            GCCy2/Ul5urvYEis9UeUpbe6tUxU0zXUWCkhMQgHeO2xQEizfIfWsUn5sYtFFoKI
            jYbAcLbRtw+Islfih8G7ydPBh78WPGz6Xx79A5nmfI1VZDAToEqpqUoaqfzsTGd1
            78GZssE3o4veTmBFvLV3Fm/ltfXpzhAIcsi89V3RjrzFM7UMD8aV153OAzhddxIu
            8x6FibmMSzBXQDFuAac2+kp9mU0F0W4G1wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    fileleech = rec {
      cores = 4;
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM+jB5QdPsAJc90alYDhAEP3sPDJb6eIj9bebj+rTBEJ fileleech";
      nets = {
        retiolum = {
          ip4.addr = "10.243.113.98";
          # ip6.addr  = "42:5cf1:e7f2:3fd:cd4c:a1ee:ec71:7096";
          aliases = [
            "fileleech.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA2W20+jYvuFUjPQ+E+7Xlabf8fW/XSnTTelfo2uRcJ3FMLYQ9H3rF
            8L8StPmxn8Q20FFH/MvRmgW8pU9z4RQ3nAi+utVYqAJQtOYA9FPMxssC08w82r0K
            YC6sgc9MeRjnCjQxQrQs4fqA6KpqSLxRf2c6kfNwYRgCxFMns2ncxOiPOoGLZait
            nJR3m0cSRm8yCTMbznlGH99+5+3HgvuBE/UYXmmGBs7w8DevaX76butzprZ8fm4z
            e5C7R9ofdVW70GGksfSI81y5xODWMbfjTRHKm4OBX7NOCiOTwx1wu8bYDN3EzN6V
            UM5PJfU42sViPEZmVuC8cDcP1xemHTkh9QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    pnp = {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.210";
          ip6.addr = "42:f9f1:0000:0000:0000:0000:0000:0001";
          aliases = [
            "pnp.r"
            "cgit.pnp.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAugkgEK4iy2C5+VZHwhjj/q3IOhhazE3TYHuipz37KxHWX8ZbjH+g
            Ewtm79dVysujAOX8ZqV8nD8JgDAvkIZDp8FCIK0/rgckhpTsy1HVlHxa7ECrOS8V
            pGz4xOxgcPFRbv5H2coHtbnfQc4GdA5fcNedQ3BP3T2Tn7n/dbbVs30bOP5V0EMR
            SqZwNmtqaDQxOvjpPg9EoHvAYTevrpbbIst9UzCyvmNli9R+SsiDrzEPgB7zOc4T
            TG12MT+XQr6JUu4jPpzdhb6H/36V6ADCIkBjzWh0iSfWGiFDQFinD+YSWbA1NOTr
            Qtd1I3Ov+He7uc2Z719mb0Og2kCGnCnPIwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    darth = {
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.84";
          ip6.addr = "42:ff6b:5f0b:460d:2cee:4d05:73f7:5566";
          aliases = [
            "darth.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA1pWNU+FY9XpQxw6srUb5mvGFgqSyJQAelFoufZng6EFeTnAzQOdq
            qT7IWN+o3kSbQQsC2tQUnRYFoPagsgFP610D+LGwmeJlNgAf23gBI9ar1agUAvYX
            yzYBj7R9OgGXHm6ECKwsxUJoGxM4L0l6mk/rTMVFnzgYPbpVJk1o6NPmiZhW8xIi
            3BfxJUSt8rEQ1OudCirvdSr9uYv/WMR5B538wg4JeQK715yKEYbYi8bqOPnTvGD8
            q5HRwXszWzCYYnqrdlmXzoCA1fT4vQdtov+63CvHT2RV7o42ruGZbHy7JIX9X3IE
            u0nA8nZhZ5byhWGCpDyr6bTkvwJpltJypQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
        siem = {
          ip4.addr   = "10.8.10.2";
          ip4.prefix = "10.8.10.0/24";
          aliases = [
            "darth.siem"
          ];
          tinc.pubkey = ''
            Ed25519PublicKey = 24t9ye4gRLg6UbVxBvuuDlvU/cnByxMjYjym4LO6GkK
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCQKCAQEApcUeTecVahqNIfLEkfgNiaW+eHQ9Y90DxHhy9vdPZh8dmLqoFBoW
            TCPcZIRpyj7hxRkNIhh34Ewpul0oQ1tzrUGcT2xvMNwaCupRDmhZn9jR9aFFEYKb
            fUOplCxb4y2UKbWAA6hie3PKH9wnPfbwSsexb2BSQAqSt4iNIVCV6j7LXpiopbGS
            Exs3/Pz+IeMtGyuMYA3rUmJsVRKR1o7axLtlhYK7JSMbqdYhaQJ4NZrvIXw//w21
            kM/TJTPZ4j47ME18jQInO62X5h+xVch6DtvwvjBMMMKbS0am9qw1P3qo7MP3PmQh
            rvVQRth8L63q4NLOnT29XmnxPSVGL1PBQQICEAE=
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    ossim = { # vm on darth
      nets = {
        siem = {
          ip4.addr = "10.8.10.6";
          ip4.prefix = "10.8.10.0/24";
          aliases = [
            "ossim.siem"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAv5qv9R3E1AHJOhTnHJ2E5zWjItRdXSw/inpz/W+KcBeM/HSG0XEl
            RyGAwty7VP4CiLp7CagWmtVsz/5ytnXJzLDeRLn5t+KzO6am0aOpvAt6ZggZXPhL
            cQkn4IGi1TJE5tw+lzabBkUZm3zD1KEXpqJeZ6spA4e9lB/+T3Tx23g9WDEOKand
            mAJrsdsvTCIiVJefidOAmgeZVVOV3ltBonNP1nqEy+5v4B3EBT/Uj7ImL2aRj/pd
            dPs6dGV2LqSQvnrSbFZzuKVXKpD1M+wgT/5NQk/hVJJxBQC6rxvpg1XyQkepcLWL
            WjvogOl4NjXStmKDX2+gPPFx6XTmwDenOwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    honeydrive = { # vm on darth
      nets = {
        internet = { # via shoney
          ip4.addr = "64.137.234.232";
          aliases = [
            "honeydrive.i"
          ];
        };
      };
    };
    tsp = {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.212";
          ip6.addr = "42:f9f1:0000:0000:0000:0000:0000:0002";
          aliases = [
            "tsp.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAwW+RjRcp3uarkfXZ+FcCYY2GFcfI595GDpLRuiS/YQAB3JZEirHi
            HFhDJN80fZ9qHqtq9Af462xSx+cIb282TxAqCM1Z9buipOcYTYo0m8xIqkT10dB3
            mR87B+Ed1H6G3J6isdwEb9ZMegyGIIeyR53FJQYMZXjxdJbAmGMDKqjZSk1D5mo+
            n5Vx3lGzTuDy84VyphfO2ypG48RHCxHUAx4Yt3o84LKoiy/y5E66jaowCOjZ6SqG
            R0cymuhoBhMIk2xAXk0Qn7MZ1AOm9N7Wru7FXyoLc7B3+Gb0/8jXOJciysTG7+Gr
            Txza6fJvq2FaH8iBnfezSELmicIYhc8Ynlq4xElcHhQEmRTQavVe/LDhJ0i6xJSi
            aOu0njnK+9xK+MyDkB7n8dO1Iwnn7aG4n3CjVBB4BDO08lrovD3zdpDX0xhWgPRo
            ReOJ3heRO/HsVpzxKlqraKWoHuOXXcREfU9cj3F6CRd0ECOhqtFMEr6TnuSc8GaE
            KCKxY1oN45NbEFOCv2XKd2wEZFH37LFO6xxzSRr1DbVuKRYIPjtOiFKpwN1TIT8v
            XGzTT4TJpBGnq0jfhFwhVjfCjLuGj29MCkvg0nqObQ07qYrjdQI4W1GnGOuyXkvQ
            teyxjUXYbp0doTGxKvQaTWp+JapeEaJPN2MDOhrRFjPrzgo3aW9+97UCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    x = {
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.91";
          ip6.addr = "42:0b2c:d90e:e717:03dc:9ac1:7c30:a4db";
          aliases = [
            "x.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAnztrijsfao+fmNtwAjqwIDKsRaMP3ECsq2T2zqKvxwCyXk69G9bG
            RFhWjgaawS9ZhnHSlgWK/vtoR0O9NxpzdU/mvdQijbVGxM02DegjO9qDSIe8EGmA
            kscW4nDqYtw4rtjOVPfnNiWXbcWD8eiYR0kcSWmSvfOpVvdhTETqduTx5HRHyEFD
            JRQYR/tJSvVWXmM670PENAPNJFJ4VSJR60s5A+bFT7J/uw7HzJXX28LygJz73Dj2
            2a4ev0WcZQngLq072h/91R/TOpg+ogUDVhXkQtKyFj7im0287JTL4bXGofZBhzaf
            +h9dFGs1QLoNyhG/cgt9fog7boSXTelAiQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
        siem = {
          ip4.addr = "10.8.10.4";
          ip4.prefix = "10.8.10.0/24";
          aliases = [
            "makefu.siem"
          ];
          tinc.pubkey = ''
            Ed25519PublicKey = rFTglGxm563e/w82Q9Qqy/E+V/ipT4DOTyTuYrWrtmI
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCQKCAQEAx+OQXQj6rlXIByo48JZXSexRz5G5oJVZTHAJ0GF5f70U65C0x83p
            XtNp4LGYti+cyyzmQjf/N7jr2CxUlOATN2nRO4CT+JaMM2MoqnPWqTZBPMDiHq2y
            ce0zjLPPl0hVc5mg+6F0tgolbUvTIo2CgAIl5lNvJiVfmXRSehmMprf1NPkxJd/O
            vAOD7mgnCjkEAWElf1cfxSGZqSLbNltRK340nE5x6A5tY7iEueP/r9chEmOnVjKm
            t+GJAJIe1PClWJHJYAXF8I7R3g+XQIqgw+VTN3Ng5cS5W/mbTFIzLWMZpdZaAhWR
            56pthtZAE5FZ+4vxMpDQ4yeDu0b6gajWNQICEAE=
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHDM0E608d/6rGzXqGbNSuMb2RlCojCJSiiz6QcPOC2G root@x";

    };

    vbob = {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.1.91";
          ip6.addr = "42:0b2c:d90e:e717:03dd:9ac1:0000:a400";
          aliases = [
            "vbob.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA+0TIo0dS9LtSdrmH0ClPHLO7dHtV9Dj7gaBAsbyuwxAI5cQgYKwr
            4G6t7IcJW+Gu2bh+LKtPP91+zYXq4Qr1nAaKw4ajsify6kpxsCBzknmwi6ibIJMI
            AK114dr/XSk/Pc6hOSA8kqDP4c0MZXwitRBiNjrWbTrQh6GJ3CXhmpZ2lJkoAyNP
            hjdPerbTUrhQlNW8FanyQQzOgN5I7/PXsZShmb3iNKz1Ban5yWKFCVpn8fjWQs5o
            Un2AKowH4Y+/g8faGemL8uy/k5xrHSrn05L92TPDUpAXrcZXzo6ao1OBiwJJVl7s
            AVduOY18FU82GUw7edR0e/b2UC6hUONflwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICPLTMl+thSq77cjYa2XF7lz5fA7JMftrLo8Dy/OBXSg root@nixos";
    };
    pigstarter = rec {
      cores = 1;

      extraZones = {
        "krebsco.de" = ''
          euer              IN MX 1   aspmx.l.google.com.
          nixos.unstable    IN CNAME  krebscode.github.io.
          gold              IN A      ${nets.internet.ip4.addr}
          boot              IN A      ${nets.internet.ip4.addr}
        '';
      };
      nets = {
        internet = {
          ip4.addr = "192.40.56.122";
          ip6.addr = "2604:2880::841f:72c";
          aliases = [
            "pigstarter.i"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.0.153";
          ip6.addr = "42:9143:b4c0:f981:6030:7aa2:8bc5:4110";
          aliases = [
            "pigstarter.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA/efJuJRLUIZROe3QE8WYTD/zyNGRh9I2/yw+5It9HSNVDMIOV1FZ
            9PaspsC+YQSBUQRN8SJ95G4RM6TIn/+ei7LiUYsf1Ik+uEOpP5EPthXqvdJEeswv
            3QFwbpBeOMNdvmGvQLeR1uJKVyf39iep1wWGOSO1sLtUA+skUuN38QKc1BPASzFG
            4ATM6rd2Tkt8+9hCeoePJdLr3pXat9BBuQIxImgx7m5EP02SH1ndb2wttQeAi9cE
            DdJadpzOcEgFatzXP3SoKVV9loRHz5HhV4WtAqBIkDvgjj2j+NnXolAUY25Ix+kv
            sfqfIw5aNLoIX4kDhuDEVBIyoc7/ofSbkQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    wry = rec {
      cores = 1;
      extraZones = {
        "krebsco.de" = ''
          wry               IN A      ${nets.internet.ip4.addr}
          io                IN NS     wry.krebsco.de.
          tinc              IN A      ${nets.internet.ip4.addr}
        '';
      };
      nets = rec {
        internet = {
          ip4.addr = "104.233.87.86";
          aliases = [
            "wry.i"
          ];
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.29.169";
          ip6.addr = "42:6e1e:cc8a:7cef:827:f938:8c64:baad";
          aliases = [
            "wry.r"
            "graph.wry.r"
            "paste.wry.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAs9bq++H4HF8EpZMfWGfoIsh/C+YNO2pg74UPBsP/tFFe71yzWwUn
            U9LW0n3bBqCMQ/oDthbSMwCkS9JzcUi22QJEdjbQs/aay9gZR115b+UxWPocw0Ms
            ZoREKo3Oe0hETk7Ing8NdBDI0kCBh9QnvqQ3iKd0rBae3DYvcWlDsY93GLGMddgA
            7E9oa3EHVYH/MPZaeJtTknaJduanBSbiEb/xQOqxTadHoQASKU6DQD1czMH3hLG2
            8Wn4MBj9fgKBAoIy092tIzPtE2QwAHO73yz4mSW/3r190hREgVbjuEPiw4w5mEyQ
            j+NeN3f3heFKx+GCgdWH9xPw6m6qPdqUiGUPq91KXMOhNa8lLcTp95mHdCMesZCF
            TFj7hf6y+SVt17Vo+YUL7UqnMtAm3eZZmwyDu0DfKFrdgz6MtDD+5dQp9g8VHpqw
            RfbaB1Srlr24EUYYoOBEF9CcIacFbsr+MKh+hQk5R0uEMSeAWARzxvvr69iMgdEC
            zDiu0rrRLN+CrfgkDir7pkRKxeA1lz8KpySyIZRziNg6mSHjKjih4++Bbu4N2ack
            86h84qBrA8lq2xsub4+HgKZGH2l5Y8tvlr+rx0mQKEJkT6XDKCXZFPfl2N0QrWGT
            Dv7l2vn0QMj9E6+BdRhYaO/m3+cIZ9faM851nRj/gq2OOtzW3ekrne0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH4Tjx9qK6uWtxT1HCpeC0XvDZKO/kaPygyKatpAqU6I root@wry";
    };
    filepimp = rec {
      cores = 1;
      nets = {
        lan = {
          ip4.addr = "192.168.1.12";
          aliases = [
            "filepimp.lan"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.153.102";
          ip6.addr = "42:4b0b:d990:55ba:8da8:630f:dc0e:aae0";
          aliases = [
            "filepimp.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA43w+A1TMOfugZ/CVwilJn4c36wWSjihaeVe7suZD0DSscKBcbkGg
            3dTCSTnu6Qb9sYd2mKebKXLreO6nhEEoFGsRU0yw/1h8gl7mWYEdTifPfvM5EWwS
            wkN9dJ5njwIUSRyWH7QTsLkiRJVFN2UxEwrhAbo1FJ7yuhRgAKqKJSN4yPVViZwR
            oHyyobvm/i2J+XSiDI9MRo74vNjnDLvO7R6ErIrhOPP1bD9fx3u+UYUfgS0iCO3X
            UN0duBz/faRcl6IRytZOuHaIp30eJ4850ZK8RPz/Dqqj+USMFq60i0oMsuAi/ljB
            8b+eQBt6OXu4MSntxoR8Ja7ht+EOTDnBOwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    omo = rec {
      cores = 2;

      nets = {
        lan = {
          ip4.addr = "192.168.1.11";
          aliases = [
            "omo.lan"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.0.89";
          ip6.addr = "42:f9f0::10";
          aliases = [
            "omo.r"
            "logs.makefu.r"
            "stats.makefu.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAuHQEeowvxRkoHJUw6cUp431pnoIy4MVv7kTLgWEK46nzgZtld9LM
            ZdNMJB9CuOVVMHEaiY6Q5YchUmapGxwEObc0y+8zQxTPw3I4q0GkSJqKLPrsTpkn
            sgEkHPfs2GVdtIBXDn9I8i5JsY2+U8QF8fbIQSOO08/Vpa3nknDAMege9yEa3NFm
            s/+x+2pS+xV6uzf/H21XNv0oufInXwZH1NCNXAy5I2V6pz7BmAHilVOGCT7g2zn6
            GasmofiYEnro4V5s8gDlQkb7bCZEIA9EgX/HP6fZJQezSUHcDCQFI0vg26xywbr6
            5+9tTn8fN2mWS5+Pdmx3haX1qFcBP5HglwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPTBGboU/P00yYiwYje53G0oqDFWmcSJ+hIpMsl4f/HH";
    };
    wbob = rec {
      cores = 1;
      nets = {
        siem = {
          ip4.addr = "10.8.10.7";
          ip4.prefix = "10.8.10.0/24";
          aliases = [ "display.siem" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA+/TpxsVIBL9J9QAe/+jB6sgu/O6J+KY4YrAzZ6dM4kbFv5JA64f5
            6znv8EFqn6loS9Aez3e08P5scyGjiwWytdKN5Yztlffc0xDD7MUU2RiCsQF1X74J
            +1i8NhSq3PJ6UeUURxYYnAYzBlFvsxev4vpniFTsIR9tmcAYX9NT9420D6nV7xq7
            FdkoBlYj4eUQqQzHH1T/Lmt+BGmf+BufIJas+Oo/Sg59vIk9OM08WyAjHVT2iNbg
            LXDhzVaeGOOM3GOa0YGG0giM3Rd245YPaPiVbwrMy8HQRBpMzXOPjcC1nYZSjxrW
            LQxtRS+dmfEMG7MJ8T2T2bseX6z6mONc1QIDAQAB
            -----END RSA PUBLIC KEY-----
            -----BEGIN ED25519 PUBLIC KEY-----
            3JGeGnADWR+hfb4TEoHDyopEYgkfGNJKwy71bqcsNrO
            -----END ED25519 PUBLIC KEY-----
          '';
        };
        retiolum = {
          ip4.addr = "10.243.214.15";
          ip6.addr = "42:5a02:2c30:c1b1:3f2e:7c19:2496:a732";
          aliases = [
            "wbob.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAqLTJx91OdR0FlJAc2JGh+AJde95oMzzh8o36JBFpsaN7styNfD3e
            QGM/bDXFjk4ieIe5At0Z63P2KWxRp3cz8LWKJsn5cGsX2074YWMAGmKX+ZZJNlal
            cJ994xX+8MJ6L2tVKpY7Ace7gqDN+l650PrEzV2SLisIqOdxoBlbAupdwHieUBt8
            khm4NLNUCxPYUx2RtHn4iGdgSgUD/SnyHEFdyDA17lWAGfEi4yFFjFMYQce/TFrs
            rQV9t5hGaofu483Epo6mEfcBcsR4GIHI4a4WKYANsIyvFvzyGFEHOMusG6nRRqE9
            TNs2RYfwDy/r6H/hDeB/BSngPouedEVcPwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    gum = rec {
      cores = 2;

      extraZones = {
        "krebsco.de" = ''
          share.euer        IN A      ${nets.internet.ip4.addr}
          mattermost.euer   IN A      ${nets.internet.ip4.addr}
          gum               IN A      ${nets.internet.ip4.addr}
          wikisearch        IN A      ${nets.internet.ip4.addr}
          pigstarter        IN A      ${nets.internet.ip4.addr}
          cgit.euer         IN A      ${nets.internet.ip4.addr}
          euer              IN A      ${nets.internet.ip4.addr}
          o.euer            IN A      ${nets.internet.ip4.addr}
          git.euer          IN A      ${nets.internet.ip4.addr}
          dl.euer           IN A      ${nets.internet.ip4.addr}
          boot.euer         IN A      ${nets.internet.ip4.addr}
          wiki.euer         IN A      ${nets.internet.ip4.addr}
          graph             IN A      ${nets.internet.ip4.addr}
          ghook             IN A      ${nets.internet.ip4.addr}
        '';
      };
      nets = rec {
        internet = {
          ip4.addr = "188.68.40.19";
          ip6.addr = "2a03:4000:17:2df::1";
          aliases = [
            "gum.i"
          ];
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.0.211";
          ip6.addr = "42:f9f0:0000:0000:0000:0000:0000:70d2";
          aliases = [
            "gum.r"
            "cgit.gum.r"
            "o.gum.r"
            "tracker.makefu.r"

            "graph.r"
            "search.makefu.r"
            "wiki.makefu.r"
            "wiki.gum.r"
            "blog.makefu.r"
            "blog.gum.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAvgvzx3rT/3zLuCkzXk1ZkYBkG4lltxrLOLNivohw2XAzrYDIw/ZY
            BTDDcD424EkNOF6g/3tIRWqvVGZ1u12WQ9A/R+2F7i1SsaE4nTxdNlQ5rjy80gO3
            i1ZubMkTGwd1OYjJytYdcMTwM9V9/8QYFiiWqh77Xxu/FhY6PcQqwHxM7SMyZCJ7
            09gtZuR16ngKnKfo2tw6C3hHQtWCfORVbWQq5cmGzCb4sdIKow5BxUC855MulNsS
            u5l+G8wX+UbDI85VSDAtOP4QaSFzLL+U0aaDAmq0NO1QiODJoCo0iPhULZQTFZUa
            OMDYHHfqzluEI7n8ENI4WwchDXH+MstsgwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcxWFEPzke/Sdd9qNX6rSJgXal8NmINYajpFCxXfYdj root@gum";
    };
    shoney = rec {
      cores = 1;
      nets = rec {
        siem = {
          via = internet;
          ip4.addr = "10.8.10.1";
          ip4.prefix = "10.8.10.0/24";
          aliases = [
            "shoney.siem"
            "graph.siem"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA0OK28PHsMGMxAqVRiRGv93zzEWJgV3hMFquWrpbYC3OZwHDYcNHu
            74skwRRwwnbcq0ZtWroEvUTmZczuPt2FewdtuEutT7uZJnAYnzSOrB9lmmdoXKQU
            l4ho1LEf/J0sMBi7RU/OJosuruQTAl53ca5KQbRCXkcPlmq4KzUpvgPINpEpYQjB
            CGC3ErOvw2jXESbDnWomYZgJl3uilJUEYlyQEwyWVG+fO8uxlz9qKLXMlkoJTbs4
            fTIcxh7y6ZA7QfMN3Ruq1R66smfXQ4xu1hybvqL66RLiDQgH3BRyKIgobS1UxI4z
            L+xhIsiMXQIo2hv8aOUnf/7Ac9DXNR83GwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.port = 1655;
        };
        internet = {
          ip4.addr = "64.137.234.215";
          aliases = [
            "shoney.i"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.205.131";
          ip6.addr = "42:490d:cd82:d2bb:56d5:abd1:b88b:e8b4";
          aliases = [
            "shoney.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAsYXzbotmODJqos+Ilve8WyO2qBti6eMDSOP59Aqb18h8A5b4tCTL
            ygDo2xLLzRaINQAxfdaKcdMOWSEkiy1j/pBYs1tfqv4mT6BO+1t8LXz82D+YcT+4
            okGXklZ/H5L+T9cynbpKIwzTrw0DuOUhzs/WRFJU60B4cJ0Tl3IQs5ePX1SevVht
            M5n1ob47SCHxEuC+ZLNdLc6KRumcp3Ozk6Yxj3lZ0tqyngxY1C+1kTJwRyw9A7vO
            +DAH8t1YusYi7ICHcYt5J1p0ZGizcs8oEnZLBy4D+bJX86g7zbix1lZ37LxDCpQ5
            uCoAYFes7QqLVDYhucZ5ElRWdATM2mBtZwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    sdev = rec {
      cores = 1;
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILtm6ETzNgLcXNkrKs2VUEiGsTKBmOFpW2fazbzdUfOg sdev";
      nets = {
        retiolum = {
          ip4.addr = "10.243.83.237";
          ip6.addr  = "42:af50:99cf:c185:f1a8:14d5:acb:8101";
          aliases = [
            "sdev.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA8BwHwQ4pLZpskVnQONJsmzRPll4ZKMjAC56sY5p+GfT9ZBMkVDn+
            LeH9wuTRiX/ehgtBiyu8w37cz62hz/71H+3mnWJlTm9bbBTc5N0y8l9b+YYeExW4
            XPm4bUbJWKNRG9tHQAns/OREYDsHLsY6UoyNFmB0wTDpgs7egDCoe7E2eT+pG428
            ysCDYlaZaigOyW+bj/HFLj8FSfpF5C/ug7NE/D7QocadsRUiLtVYrJsfmT+KHWf+
            f5rLWLvuFiz1SWf7wZ9sICF3RCaC9Qhz7zplgHbvwbOHtF+Z/6DxduRMkggZUsUD
            nm+40Ex1XJTe+s4V4GKLgh/fDKBTS6JwewIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };


# non-stockholm

    flap = rec {
      cores = 1;
      extraZones = {
        "krebsco.de" = ''
          mediengewitter    IN A      ${nets.internet.ip4.addr}
          flap              IN A      ${nets.internet.ip4.addr}
        '';
      };
      nets = {
        internet = {
          ip4.addr = "162.248.11.162";
          aliases = [
            "flap.i"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.211.172";
          ip6.addr = "42:472a:3d01:bbe4:4425:567e:592b:065d";
          aliases = [
            "flap.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAwtLD+sgTQGO+eh2Ipq2r54J1I0byvfkaTBeBwhtUmWst+lUQUoGy
            2fGReRYsb4ThDLeyK439jZuQBeXSc5r2g0IHBJCSWj3pVxc1HRTa8LASY7QuprQM
            8rSQa2XUtx/KpfM2eVX0yIvLuPTxBoOf/AwklIf+NmL7WCfN7sfZssoakD5a1LGn
            3EtZ2M/4GyoXJy34+B8v7LugeClnW3WDqUBZnNfUnsNWvoldMucxsl4fAhvEehrL
            hGgQMjHFOdKaLyatZOx6Pq4jAna+kiJoq3mVDsB4rcjLuz8XkAUZmVpe5fXAG4hr
            Ig8l/SI6ilu0zCWNSJ/v3wUzksm0P9AJkwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    nukular = rec {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.231.219";
          ip6.addr = "42:f7bf:178d:4b68:1c1b:42e8:6b27:6a72";
          aliases = [
            "nukular.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAnt/d9Ys9gmQMGEPzPydAs0Etp9aPb5PreogzVilvazFCZ8HiQHl/
            gRGlNBImcPPAPGgLjQ49TZ6V1s0bX0GMlu9gJxqU7Nz/TPbAaDJSmEDPkXnaMC97
            gLoluwJHURKPP6+0VNQuK/IOjjDLzLjRDiVeIg6NR0nFAQPlxUhrCN/PhxqNV5WP
            H1nR+a4UDoLcKbtgQP+4Eu09iEm+H6o5eCFTX2Ov9Ok2m948Jm0rAqUbPAISf9m4
            tOOhhUhn0xvQy5iNHI72ndLvogQ968rnFwBpZM7HF1FsiaQfOF9Nhf11rHCJod3P
            meq9GsIUyppZmEKecnTtVfG1oUHMbt1GxQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    heidi = rec {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.124.21";
          ip6.addr = "42:9898:a8be:ce56:0ee3:b99c:42c5:109e";
          aliases = [
            "heidi.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAqRLnAJNZ1OoO1bTS58DQgxi1VKgITHIuTW0fVGDvbXnsjPUB3cgx
            1GEVtLc0LN6R9wrPKDaqHS6mkiRSDVScaW/FqkdFhTDaBJy8LfomL9ZmkU9DzkvQ
            jncDjr0WoR+49rJHYsUULp1fe98Ev+y3VwVdJOOH92pAj1CAAUdtfG7XcGyHznYY
            ZNLriGZe3l1AwsWMEflzHLeXcKQ/ZPOrjZ4EFVvfGfdQdJ24UUF3r4sBypYnasmA
            q8lCw9rCrFh1OS6mHLC9qsvGfal6X4x2/xKc5VxZD4MQ/Bp7pBi1kwfHpKoREFKo
            w/Jr3oG/uDxMGIzphGX185ObIkZ1wl/9DwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };


    lariat = rec {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.64.7";
          aliases = [
            "lariat.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAqiDzxADQYY8cWBH+R5aKSoxaFHLvPvVMgB7R1Y6QVTqD5YUCuINX
            eBLFV9idHnHzdZU+xo/c8EFQf0hvyP0z3bcXaiw+RlpEYdK6tuaypJ3870toqWmA
            269H8ufA3DA0hxlY7dwnhg8Rb7KGIlNN8fy4RMGe73PupF5aAmiDiEhPalv4E0qJ
            unmk5y1OHQFPxYm++yLo5SVFlcO89jDtGpvg5papp8JvtxTkrshby1lXf/sph3Cv
            d1z6h7S+HgT+BMwTZY5dIrwYAcob/t1sRmWsY62P1n02RbiJFm27wg0t/ZcfsI2o
            yBjRTiK5ACJaIdpM99/902gJsuJASPGB2QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    soundflower = rec {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.69.184";
          aliases = [
            "soundflower.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA0a0oenAy9MDa2M6NoLtB8elduGgc3oLtUwsm3iUu6w8L+Je5TndN
            H8dPn3sByUk1Jkd8tGGRk/vSFj/mtUn7xXKCnFXfKDqVowu/0KS3Q+6o4mcoATeb
            Ax7e6Cz1YH5+qhQjR7apuase9X9Dzp56//5VW2gaScvWevvzrij2x7eNvJRF+W/l
            FDXc8zBPkFW5TLFHOizRoLl4mK1hz2NrUiqcq5Ghs2yPsFxl/o5+e2MOwtdI49T6
            lMkeshAeNOSMKYfP9nmHZoKI/MIpGak0EF3ZQtLvyv+tM2Q0nuwH3RvxlK/Xf6U+
            8SoQu4yRIeK+pMiLEHhFPzBpk+sblUlG7QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    falk = rec {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.120.19";
          aliases = [
            "falk.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA961eCQE562VPYjuZtd0+FNRfUghvD2ccjUlihMjzg46GAK+duqK+
            4peWklGOL4eRYQBg6G2VDzWiU2MxXVbXUZaMrxh7fTc3G3LdbqTxzAv3GQKR/6iA
            9bGUf6u4ztVNAcj2mrY3mfs4gMlBQyQ2wcM0ZUpiAMaRB4cdq7I4GVHbYTFYfQuI
            2zdnr0w8AjlMpFFcD0ExsWeppiJsE7iiME/S2VVfh2NrEpAKQbLH9fKrfkiJA/+9
            0VIH9wLLIYngUtQKbvEQ5xgx6ybrg0vO8ZqZ1ZGXYxOQZzWzPP0tvDU0QHSKYSWb
            FjcOf1lWSWjsjHxMl/Gh57hjNJFCbs8yjQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    filebitch = rec {
      cores = 4;
      nets = {
        retiolum = {
          ip4.addr = "10.243.189.130";
          ip6.addr = "42:c64e:011f:9755:31e1:c3e6:73c0:af2d";
          aliases = [
            "filebitch.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA2VjW30A3uQoo5QwbFTnl5fuGg81DZVu8HXmDwgEkhZYr5Xf3V5/d
            fmPlX1igzatWYX0OylFAY69r0V4dqeTubIf83sz1eqtpXjK4czG8A3wMHEXj5Pzs
            e1Qh8K4rHMEATc7Y/cwpQBi2THn2bhufqgaz94m8HrStCZcKCin3fDMbE01WHWX1
            KFqeBtUd7b9pWbXKlLBNpHTZoGxVQk0Hto9pxYzHecRsbQXykYk3Rw2tSuf0aH99
            oY0i3LjOb+f2oq2S4qVHqHZsMJfDVr+x2/LP1SIcc1lVTztWSSAzZEokE0/ejvXf
            wkquBVHXdl6LuzH+/V1I7OsaMhHShYu1LwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    bridge = rec {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.26.29";
          ip6.addr = "42:927a:3d59:1cb3:29d6:1a08:78d3:812e";
          aliases = [
            "excobridge.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEApeeMSYMuXg4o/fNHnG2ftp2WskZLrt63zhRag7U1HqYUnuPqY60d
            VVy9MBTawm6N02nC2Svm3V07ZXaRp/XsXQLx+evZcDjPjnDYgl2ZGX0ir5Cn50bm
            UzhJiMW6/J7AYvucgeAaVJ0YmIwRw6ndYGcxmXWi4TK0jSzhuSLgookWM6iJfbdB
            oaYsjiXisEvNxt7rBlCfacaHMlPhz3gr1gc4IDCwF+RAMM29NUN3OinI+/f56d7b
            /hLZWbimiwtvGVsGLiA2EIcfxQ7aD/LINu+XXMaq7f8QByXj/Lzi7456tDi3pdJg
            lyg9yqRJYt4Zle5PVejn08qiofTUmlEhnwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    tahoe = rec {
      cores = 1;
      nets = {
        internet = {
          ip4.addr = "148.251.47.69";
          aliases = [
            "wooki.i"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.57.85";
          ip6.addr = "42:2f06:b899:a3b5:1dcf:51a4:a02b:8731";
          aliases = [
            "wooki.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAx6R+CuJu4Bql+DgGPpE7wI+iasRY6ltxW0/L04uW9XiOKiEjx66y
            QMMaW18bcb0SOfTE8qYo8pOsZ5E9FFPY6cKH4DGi8g1FpaODle9V8RrVg3F7RuZ8
            dXDXeZxvYvJ2LwPBvlr1aisqJqgxAwF2ipPPX97rAYbp46a/vkgU5bPF1OFlTDaH
            9jjThuidiEwY4EMtJGKisnTGx8yS5iQibDMqzrcRpCxCLcl68FgFNKCTtSIj1mo6
            hgO1ZKmHw73ysmrL2tImmalHYcqDJnq/KInG2ZkCZI/2ZqfJyrRSTk86t5ubfD6p
            egC5N0Y5dQHJd66AytNwXxymiAcWuYth9QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    senderechner = rec {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.163";
          ip6.addr = "42:b67b:5752:a730:5f28:d80d:6b37:5bda";
          aliases = [
            "senderechner.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA0zCc5aLVRO6NuxUoR6BVzq2PQ/U5AEjYTdGkQufRot42N29MhxY7
            lJBfPfkw/yg2FOzmAzTi62QyrLWSaF1x54rKu+JeNSsOAX+BorGhM67N45DGvJ0X
            rakIL0BrVoV7Kxssq3DscGVbjbNS5B5c+IvTp97me/MpuDrfYqUyZk5mS9nB0oDL
            inao/A5AtOO4sdqN5BNE9/KisN/9dD359Gz2ZGGq6Ki7o4HBdBj5vi0f4fTofZxT
            BJH4BxbWaHwXMC0HYGlhQS0Y7tKYT6h3ChxoLDuW2Ox2IF5AQ/O4t4PIBDp1XaAO
            OK8SsmsiD6ZZm6q/nLWBkYH08geYfq0BhQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    tcac-0-1 = rec {
      cores = 1;
      ssh.privkey.path = <secrets/ssh_host_ed25519_key>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcX7rlGmGp1zCStrERXZ3XuT/j69FDBXV4ceLn9RXsG tcac-0-1
        ";
      nets = {
        retiolum = {
          ip4.addr = "10.243.144.142";
          ip6.addr  = "42:4bf8:94b:eec5:69e2:c837:686e:f278";
          aliases = [
            "tcac-0-1.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA+3zuZa8FhFBcUNdNGyTQph6Jes0WDQB4CDcEcnK9okP60Z0ONq8j
            7sKmxzQ43WFm04fd992Aa/KLbYBbXmGtYuu68DQwQGwk3HVNksp6ha7uVK1ibgNs
            zJIKizpFqK4NAYit0OfAy7ugVSvtyIxg9CDhnASDZ5NRq8/OLhvo5M4c3r3lGOlO
            Hv1nf4Tl2IYRln3c+AJEiw2369K46mRlt28yHeKUw1ur6hrbahnkYW+bjeliROIs
            QLp8J8Jl6evtPOyZpgyGHLQ/WPsQRK5svVA9ou17R//m4KNL1kBjTfxs7GaJWHLl
            HpSZTqRKsuK6K9R6kzu7NU81Wz0HXxw/qwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
  } // { # hosts only maintained in stockholm, not owned by me
    muhbaasu = rec {
      owner = config.krebs.users.root;
      cores = 1;
      nets = {
        internet = {
          ip4.addr = "217.160.206.154";
          aliases = [
            "muhbaasu.i"
          ];
        };
        retiolum = {
          ip4.addr = "10.243.139.184";
          ip6.addr = "42:d568:6106:ba30:753b:0f2a:8225:b1fb";
          aliases = [
            "muhbaasu.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA0f4C4xKXpnyV1ig03O2Kef8ag+/5WGkW90uxEBb/h5NY9barex+Z
            KqVbkPdHhwoCIINuCVcOnJXzeo0FZtSEq3zVhscVm0PVdNfjct8a9KMsK0iUmuul
            5WD9Glh5/1wkEmbRfVxDErhssz1b8YmFOAGQn+ujO/Znn3BLv36uKQvpqU2y5bzb
            +rVnq3eE1bCSeuj41bgEve8+vxpforjLO6gbE91mwp3Ol6nkkp6CjpG+aFTuLCAj
            YR0MIl2gGwskOGSI38QxlLouOlIGwus5f+KfC94ZP0pMwu5pT45UOUkVnlBXuZ9E
            igNHG2Vtm76nB3yYHndOvuDTOufatX61dQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    tpsw = {
      cores = 2;
      owner = config.krebs.users.ciko; # main laptop
        nets = {
          retiolum = {
            ip4.addr = "10.243.183.236";
            ip6.addr = "42:8ca8:d2e4:adf6:5c0f:38cb:e9ef:eb3c";
            aliases = [
              "tpsw.r"
            ];
            tinc.pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEAvwYPFAINwV0EH0myFpNzRjVbqXdAmJP616C5JvODklhZWJxFxlKJ
              Poczl57j2Z+4bonkTrJmsNtSaQLPKYH4H1qfo/lwz7nqEpPi3Xp4Fgts23w36eML
              WBvbw0fQO9R8zZJIIdRkJ2qqlhZiTlor1Gtlm8Z1RmpKkhL9O6Yzj94VhGLhABVl
              OsaF2M3PgXJMiLry67jzbAs3+mVaT3iBTzWOaOyREjKQEUg9B9IDxrmZMSWqdXZM
              0wfzaCjS40jD73m7tqi7W3tXzAUP4mEeUqkC+NC2Zgm/lJ5B1KPx7AyNqtRLsBLd
              pIdJs6ng63WV1fyHYUWMYqZk9zB/tQ0b0wIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
          };
        };
    };
  };
  users = rec {
    makefu = {
      mail = "makefu@x.r";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCl3RTOHd5DLiVeUbUr/GSiKoRWknXQnbkIf+uNiFO+XxiqZVojPlumQUVhasY8UzDzj9tSDruUKXpjut50FhIO5UFAgsBeMJyoZbgY/+R+QKU00Q19+IiUtxeFol/9dCO+F4o937MC0OpAC10LbOXN/9SYIXueYk3pJxIycXwUqhYmyEqtDdVh9Rx32LBVqlBoXRHpNGPLiswV2qNe0b5p919IGcslzf1XoUzfE3a3yjk/XbWh/59xnl4V7Oe7+iQheFxOT6rFA30WYwEygs5As//ZYtxvnn0gA02gOnXJsNjOW9irlxOUeP7IOU6Ye3WRKFRR0+7PS+w8IJLag2xb makefu@x";
      pgp.pubkeys.default = builtins.readFile ./pgp/default.asc;
      pgp.pubkeys.brain = builtins.readFile ./pgp/brain.asc;
    };
    makefu-omo = {
      inherit (makefu) mail pgp;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAtDhAxjiCH0SmTGNDqmlKPug9qTf+IFOVjdXfk01lAV2KMVW00CgNo2d5kl5+6pM99K7zZO7Uo7pmSFLSCAg8J6cMRI3v5OxFsnQfcJ9TeGLZt/ua7F8YsyIIr5wtqKtFbujqve31q9xJMypEpiX4np3nLiHfYwcWu7AFAUY8UHcCNl4JXm6hsmPe+9f6Mg2jICOdkfMMn0LtW+iq1KZpw1Nka2YUSiE2YuUtV+V+YaVMzdcjknkVkZNqcVk6tbJ1ZyZKM+bFEnE4VkHJYDABZfELpcgBAszfWrVG0QpEFjVCUq5atpIVHJcWWDx072r0zgdTPcBuzsHHC5PRfVBLEw== makefu@servarch";
    };
    makefu-tsp = {
      inherit (makefu) mail pgp;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC1srWa67fcsw3r64eqgIuHbMbrj6Ywd9AwzCM+2dfXqYQZblchzH4Q4oydjdFOnV9LaA1LfNcWEjV/gVQKA2/xLSyXSDwzTxQDyOAZaqseKVg1F0a7wAF20+LiegQj6KXE29wcTW1RjcPncmagTBv5/vYbo1eDLKZjwGpEnG0+s+TRftrAhrgtbsuwR1GWWYACxk1CbxbcV+nIZ1RF9E1Fngbl4C4WjXDvsASi8s24utCd/XxgKwKcSFv7EWNfXlNzlETdTqyNVdhA7anc3N7d/TGrQuzCdtrvBFq4WbD3IRhSk79PXaB3L6xJ7LS8DyOSzfPyiJPK65Zw5s4BC07Z makefu@tsp";
    };
    makefu-vbob = {
      inherit (makefu) mail pgp;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCiKvLKaRQPL/Y/4EWx3rNhrY5YGKK4AeqDOFTLgJ7djwJnMo7FP+OIH/4pFxS6Ri2TZwS9QsR3hsycA4n8Z15jXAOXuK52kP65Ei3lLyz9mF+/s1mJsV0Ui/UKF3jE7PEAVky7zXuyYirJpMK8LhXydpFvH95aGrL1Dk30R9/vNkE9rc1XylBfNpT0X0GXmldI+r5OPOtiKLA5BHJdlV8qDYhQsU2fH8S0tmAHF/ir2bh7+PtLE2hmRT+b8I7y1ZagkJsC0sn9GT1AS8ys5s65V2xTTIfQO1zQ4sUH0LczuRuY8MLaO33GAzhyoSQdbdRAmwZQpY/JRJ3C/UROgHYt makefu@vbob";
    };
    makefu-tempx = {
      inherit (makefu) mail pgp;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDOXG6iwvm6zUVk+OE9ZviO+WNosAHSZw4ku0RxWbXSlSG0RfzvV4IfByF3Dw+4a8yZQmjwNkQalUURh2fEqhBLBI9XNEIL7qIu17zheguyXzpE3Smy4pbI+fjdsnfFrw+WE2n/IO8N6ojdH6sMmnWwfkFZYqqofWyLB3WUN9wy2b2z0w/jc56+HxxyTl3rD7CttTs9ak67HqIn3/pNeHoOM+JQ/te8t4ageIlPi8yJJpqZgww1RUWCgPPwZ9DP6gQjo85he76x0h9jvhnFd7m9N1aGdRDcK55QyoY/9x07R24GRutohAB/KDWSkDWQv5BW7M1LCawpJcF3DDslD1i7 makefu@gum";
    };
    makefu-bob = {
      inherit (makefu) mail pgp;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC+fEK1bCB8cdDiBzXBXEWLFQyp/7xjNGQ5GyqHOtgxxe6Ypb0kAaWJaG3Ak/qI/nToGKwkQJLsuYNA3lZj2rFyBdoxnNO3kRFTc7NoaU5mC2BlHbpmn9dzvgiBoRAKAlzj/022u65SI19AFciKXtwqQfjuB3mPVOFOfCFB2SYjjWb8ffPnHp6PB5KKNLxaVPCbZgOdSju25/wB2lY00W8WIDOTqfbNClQnjkLsUZpTuRnvpHTemKtt1FH+WBZiMwMXRt19rm9LFSO7pvrZjdJz0l1TZVsODkbKZzQzSixoCPmdpPPAYaqrGUQpmukXk0xQtR3E2jEsk+FJv4AkIKqD";
    };
    ciko = {
      mail = "wieczorek.stefan@googlemail.com";
    };
    exco = {
      mail = "dickbutt@excogitation.de";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC7HCK+TzelJp7atCbvCbvZZnXFr3cE35ioactgpIJL7BOyQM6lJ/7y24WbbrstClTuV7n0rWolDgfjx/8kVQExP3HXEAgCwV6tIcX/Ep84EXSok7QguN0ozZMCwX9CYXOEyLmqpe2KAx3ggXDyyDUr2mWs04J95CFjiR/YgOhIfM4+gVBxGtLSTyegyR3Fk7O0KFwYDjBRLi7a5TIub3UYuOvw3Dxo7bUkdhtf38Kff8LEK8PKtIku/AyDlwZ0mZT4Z7gnihSG2ezR5mLD6QXVuGhG6gW/gsqfPVRF4aZbrtJWZCp2G21wBRafpEZJ8KFHtR18JNcvsuWA1HJmFOj2K0mAY5hBvzCbXGhSzBtcGxKOmTBDTRlZ7FIFgukP/ckSgDduydFUpsv07ZRj+qY07zKp3Nhh3RuN7ZcveCo2WpaAzTuWCMPB0BMhEQvsO8I/p5YtTaw2T1poOPorBbURQwEgNrZ92kB1lL5t1t1ZB4oNeDJX5fddKLkgnLqQZWOZBTKtoq0EAVXojTDLZaA+5z20h8DU7sicDQ/VG4LWtqm9fh8iDpvt/3IHUn/HJEEnlfE1Gd+F2Q+R80yu4e1PClmuzfWjCtkPc4aY7oDxfcJqyeuRW6husAufPqNs31W6X9qXwoaBh9vRQ1erZUo46iicxbzujXIy/Hwg67X8dw== dickbutt@excogitation.de";
    };
  };
}
