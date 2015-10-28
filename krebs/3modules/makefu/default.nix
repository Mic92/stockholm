{ lib, ... }:

with lib;

{
  hosts = addNames {
    pnp = {
      cores = 1;
      dc = "makefu"; #vm on 'omo'
      nets = {
        retiolum = {
          addrs4 = ["10.243.0.210"];
          addrs6 = ["42:f9f1:0000:0000:0000:0000:0000:0001"];
          aliases = [
            "pnp.retiolum"
            "cgit.pnp.retiolum"
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
    tsp = {
      cores = 1;
      dc = "makefu"; #x200
      nets = {
        retiolum = {
          addrs4 = ["10.243.0.212"];
          addrs6 = ["42:f9f1:0000:0000:0000:0000:0000:0002"];
          aliases = [
            "tsp.retiolum"
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
    pornocauster = {
      cores = 2;
      dc = "makefu"; #x220
      nets = {
        retiolum = {
          addrs4 = ["10.243.0.91"];
          addrs6 = ["42:0b2c:d90e:e717:03dc:9ac1:7c30:a4db"];
          aliases = [
            "pornocauster.retiolum"
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
    flap = rec {
      cores = 1;
      dc = "cac"; #vps

      extraZones = {
        "krebsco.de" = ''
          mediengewitter    IN A      ${head nets.internet.addrs4}
          flap              IN A      ${head nets.internet.addrs4}
        '';
      };
      nets = {
        internet = {
          addrs4 = ["162.248.11.162"];
          aliases = [
            "flap.internet"
          ];
        };
        retiolum = {
          addrs4 = ["10.243.211.172"];
          addrs6 = ["42:472a:3d01:bbe4:4425:567e:592b:065d"];
          aliases = [
            "flap.retiolum"
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
    pigstarter = rec {
      cores = 1;
      dc = "frontrange"; #vps

      extraZones = {
        "krebsco.de" = ''
          euer              IN MX 1   aspmx.l.google.com.
          pigstarter        IN A      ${head nets.internet.addrs4}
          gold              IN A      ${head nets.internet.addrs4}
          boot              IN A      ${head nets.internet.addrs4}
        '';
      };
      nets = {
        internet = {
          addrs4 = ["192.40.56.122"];
          addrs6 = ["2604:2880::841f:72c"];
          aliases = [
            "pigstarter.internet"
          ];
        };
        retiolum = {
          addrs4 = ["10.243.0.153"];
          addrs6 = ["42:9143:b4c0:f981:6030:7aa2:8bc5:4110"];
          aliases = [
            "pigstarter.retiolum"
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
      dc = "makefu"; #dc = "cac";
      extraZones = {
        "krebsco.de" = ''
          wiki.euer      IN A  ${head nets.internet.addrs4}
          wry            IN A  ${head nets.internet.addrs4}
          io             IN NS wry.krebsco.de.
          graphs         IN A  ${head nets.internet.addrs4}
          paste       60 IN A  ${head nets.internet.addrs4}
          tinc           IN A  ${head nets.internet.addrs4}
        '';
      };
      nets = rec {
        internet = {
          addrs4 = ["104.233.87.86"];
          aliases = [
            "wry.internet"
            "paste.internet"
          ];
        };
        retiolum = {
          via = internet;
          addrs4 = ["10.243.29.169"];
          addrs6 = ["42:6e1e:cc8a:7cef:827:f938:8c64:baad"];
          aliases = [
            "graphs.wry.retiolum"
            "graphs.retiolum"
            "paste.wry.retiolum"
            "paste.retiolum"
            "wry.retiolum"
            "wiki.makefu.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAvmCBVNKT/Su4v9nl/Nm3STPo5QxWPg7xEkzIs3Oh39BS8+r6/7UQ
            rebib7mczb+ebZd+Rg2yFoGrWO8cmM0VcLy5bYRMK7in8XroLEjWecNNM4TRfNR4
            e53+LhcPdkxo0A3/D+yiut+A2Mkqe+4VXDm/JhAiAYkZTn7jUtj00Atrc7CWW1gN
            sP3jIgv4+CGftdSYOB4dm699B7OD9XDLci2kOaFqFl4cjDYUok03G0AduUlRx10v
            CKbKOTIdm8C36A902/3ms+Hyzkruu+VagGIZuPSwqXHJPCu7Ju+jarKQstMmpQi0
            PubweWDL0o/Dfz2qT3DuL4xDecIvGE6kv3m41hHJYiK+2/azTSehyPFbsVbL7w0V
            LgKN3usnZNcpTsBWxRGT7nMFSnX2FLDu7d9OfCuaXYxHVFLZaNrpccOq8NF/7Hbk
            DDW81W7CvLyJDlp0WLnAawSOGTUTPoYv/2wAapJ89i8QGCueGvEc6o2EcnBVMFEW
            ejWTQzyD816f4RsplnrRqLVlIMbr9Q/n5TvlgjjhX7IMEfMy4+7qLGRQkNbFzgwK
            jxNG2fFSCjOEQitm0gAtx7QRIyvYr6c7/xiHz4AwxYzBmvQsL/OK57NO4+Krwgj5
            Vk8TQ2jGO7J4bB38zaxK+Lrtfl8i1AK1171JqFMhOc34JSJ7T4LWDMECAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    gum = rec {
      cores = 1;
      dc = "online.net"; #root-server

      extraZones = {
        "krebsco.de" = ''
          euer              IN A      ${head nets.internet.addrs4}
          share.euer        IN A      ${head nets.internet.addrs4}
          gum               IN A      ${head nets.internet.addrs4}
        '';
      };
      nets = {
        internet = {
          addrs4 = ["195.154.108.70"];
          aliases = [
            "gum.internet"
          ];
        };
        retiolum = {
          addrs4 = ["10.243.0.211"];
          addrs6 = ["42:f9f0:0000:0000:0000:0000:0000:70d2"];
          aliases = [
            "gum.retiolum"
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
    };
  };
  users = addNames {
    makefu = {
      mail = "makefu@pornocauster.retiolum";
      pubkey = readFile ../../Zpubkeys/makefu_arch.ssh.pub;
    };
  };
}
