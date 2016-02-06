{ lib, ... }:

with lib;

{
  dns.providers = {
    de.viljetic = "regfish";
  };
  hosts = addNames {
    cd = rec {
      cores = 2;
      dc = "tv"; #dc = "cac";
      extraZones = {
        # TODO generate krebsco.de zone from nets and don't use extraZones at all
        "krebsco.de" = ''
          krebsco.de. 60 IN MX 5 mx23
          mx23        60 IN A ${elemAt nets.internet.addrs4 0}
          cd          60 IN A ${elemAt nets.internet.addrs4 0}
          cgit        60 IN A ${elemAt nets.internet.addrs4 0}
          cgit.cd     60 IN A ${elemAt nets.internet.addrs4 0}
        '';
      };
      nets = rec {
        internet = {
          addrs4 = ["162.219.7.216"];
          aliases = [
            "cd.internet"
            "cd.krebsco.de"
            "cgit.cd.krebsco.de"
            "cd.viljetic.de"
            "cgit.cd.viljetic.de"
          ];
          ssh.port = 11423;
        };
        retiolum = {
          via = internet;
          addrs4 = ["10.243.113.222"];
          addrs6 = ["42:4522:25f8:36bb:8ccb:0150:231a:2af3"];
          aliases = [
            "cd.retiolum"
            "cgit.cd.retiolum"
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
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOd/HqZIO9Trn3eycl23GZAz21HQCISaVNfNyaLSQvJ6";
    };
    mkdir = rec {
      cores = 1;
      dc = "tv"; #dc = "cac";
      nets = rec {
        internet = {
          addrs4 = ["104.167.114.142"];
          aliases = [
            "mkdir.internet"
          ];
        };
        retiolum = {
          via = internet;
          addrs4 = ["10.243.113.223"];
          addrs6 = ["42:4522:25f8:36bb:8ccb:0150:231a:2af4"];
          aliases = [
            "mkdir.retiolum"
            "cgit.mkdir.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAuyfM+3od75zOYXqnqRMAt+yp/4z/vC3vSWdjUvEmCuM23c5BOBw+
            dKqbWoSPTzOuaQ0szdL7a6YxT+poSUXd/i3pPz59KgCl192rd1pZoJKgvoluITev
            voYSP9rFQOUrustfDb9qKW/ZY95cwdCvypo7Vf4ghxwDCnlmyCGz7qXTJMLydNKF
            2PH9KiY4suv15sCg/zisu+q0ZYQXUc1TcgpoIYBOftDunOJoNdbti+XjwWdjGmJZ
            Bn4GelsrrpwJFvfDmouHUe8GsD7nTgbZFtiJbKfCEiK16N0Q0d0ZFHhAV2nPjsk2
            3JhG4n9vxATBkO82f7RLrcrhkx9cbLfN3wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICuShEqU0Cdm7KCaMD5x1D6mgj+cr7qoqbzFJDKoBbbw";
    };
    ire = rec {
      extraZones = {
        # TODO generate krebsco.de zone from nets and don't use extraZones at all
        "krebsco.de" = ''
          ire 60 IN A ${elemAt nets.internet.addrs4 0}
        '';
      };
      nets = {
        internet = {
          addrs4 = ["198.147.22.115"];
          aliases = [
            "ire.internet"
            "ire.krebsco.de"
          ];
          ssh.port = 11423;
        };
        retiolum = {
          addrs4 = ["10.243.231.66"];
          addrs6 = ["42:b912:0f42:a82d:0d27:8610:e89b:490c"];
          aliases = [
            "ire.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAwofjmP/XBf5pwsJlWklkSzI+Bo0I0B9ONc7/j+zpbmMRkwbWk4X7
            rVLt1cWvTY15ujg2u8l0o6OgEbIkc6rslkD603fv1sEAd0KOv7iKLgRpE9qfSvAt
            6YpiSv+mxEMTpH0g36OmBfOJ10uT+iHDB/FfxmgGJx//jdJADzLjjWC6ID+iGkGU
            1Sf+yHXF7HRmQ29Yak8LYVCJpGC5bQfWIMSL5lujLq4NchY2d+NZDkuvh42Ayr0K
            LPflnPBQ3XnKHKtSsnFR2vaP6q+d3Opsq/kzBnAkjL26jEuFK1v7P/HhNhJoPzwu
            nKKWj/W/k448ce374k5ycjvKm0c6baAC/wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          ssh.port = 11423;
        };
      };
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBaMjBJ/BfYlHjyn5CO0xzFNaQ0LPvMP3W9UlOs1OxGY";
    };
    kaepsele = {
      nets = {
        internet = {
          addrs4 = ["92.222.10.169"];
          aliases = [
            "kaepsele.internet"
            # TODO "kaepsele.org"
          ];
        };
        retiolum = {
          addrs4 = ["10.243.166.2"];
          addrs6 = ["42:0b9d:6660:d07c:2bb7:4e91:1a01:2e7d"];
          aliases = [
            "kaepsele.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAxj7kaye4pGLou7mVRTVgtcWFjuEosJlxVg24gM7nU1EaoRnBD93/
            Y3Je7BSUbz5xMXr5SFTPSkitInL7vU+jDOf2bEpqv+uUJAJIz85494oPS9xocdWo
            rQsrQRAtOg4MLD+YIoAxQm2Mc4nt2CSE1+UP4uXGxpuh0c051b+9Kmwv1bTyHB9y
            y01VSkDvNyHk5eA+RGDiujBAzhi35hzTlQgCJ3REOBiq4YmE1d3qpk3oNiYUcrcu
            yFzQrSRIfhXjuzIR+wxqS95HDUsewSwt9HgkjJzYF5sQZSea0/XsroFqZyTJ8iB5
            FQx2emBqB525cWKOt0f5jgyjklhozhJyiwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDA9cDUg7qm37uOhQpdKSgpnJPWao9VZR6LFNphVcJQ++gYvVgWu6WMhigiy7DcGQSStUlXkZc4HZBBugwwNWcf7aAF6ijBuG5rVwb9AFQmSexpTOfWap33iA5f+LXYFHe7iv4Pt9TYO1ga1Ryl4EGKb7ol2h5vbKC+JiGaDejB0WqhBAyrTg4tTWO8k2JT11CrlTjNVctqV0IVAMtTc/hcJcNusnoGD4ic0QGSzEMYxcIGRNvIgWmxhI6GHeaHxXWH5fv4b0OpLlDfVUsIvEo9KVozoLGm/wgLBG/tQXKaF9qVMVgOYi9sX/hDLwhRrcD2cyAlq9djo2pMARYiriXF";
    };
    nomic = {
      cores = 2;
      dc = "tv"; #dc = "gg23";
      nets = rec {
        gg23 = {
          addrs4 = ["10.23.1.110"];
          aliases = ["nomic.gg23"];
        };
        retiolum = {
          addrs4 = ["10.243.0.110"];
          addrs6 = ["42:02d5:733f:d6da:c0f5:2bb7:2b18:09ec"];
          aliases = [
            "nomic.retiolum"
            "cgit.nomic.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAwb8Yk/YRc17g2J9n960p6j4W/l559OPyuMPdGJ4DmCm3WNQtxoa+
            qTFUiDiI85BcmfqnSeddLG8zTC2XnSlIvCRMJ9oKzppFM4PX4OTAaJZVE5WyCQhw
            Kd4tHVdoQgJW5yFepmT9IUmHqkxXJ0R2W93l2eSZNOcnFvFn0ooiAlRi4zAiHClu
            5Mz80Sc2rvez+n9wtC2D06aYjP23pHYld2xighHR9SUqX1dFzgSXNSoWWCcgNp2a
            OKcM8LzxLV7MTMZFOJCJndZ77e4LsUvxhQFP6nyKZWg30PC0zufZsuN5o2xsWSlA
            Wi9sMB1AUR6mZrxgcgTFpUjbjbLQf+36CwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMIHmwXHV7E9UGuk4voVCADjlLkyygqNw054jvrsPn5t root@nomic";
    };
    ok = {
      nets = {
        gg23 = {
          addrs4 = ["10.23.1.1"];
          aliases = ["ok.gg23"];
        };
      };
    };
    rmdir = rec {
      cores = 1;
      dc = "tv"; #dc = "cac";
      nets = rec {
        internet = {
          addrs4 = ["167.88.34.182"];
          aliases = [
            "rmdir.internet"
          ];
        };
        retiolum = {
          via = internet;
          addrs4 = ["10.243.113.224"];
          addrs6 = ["42:4522:25f8:36bb:8ccb:0150:231a:2af5"];
          aliases = [
            "rmdir.retiolum"
            "cgit.rmdir.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA+twy4obSbJdmZLfBoe9YYeyoDnXkO/WPa2D6Eh6jXrWk5fbhBjRf
            i3EAQfLiXXFJX3E8V8YvJyazXklI19jJtCLDiu/F5kgJJfyAkWHH+a/hcg7qllDM
            Xx2CvS/nCbs+p48/VLO6zLC7b1oHu3K/ob5M5bwPK6j9NEDIL5qYiM5PQzV6zryz
            hS9E/+l8Z+UUpYcfS3bRovXJAerB4txc/gD3Xmptq1zk53yn1kJFYfVlwyyz+NEF
            59JZj2PDrvWoG0kx/QjiNurs6XfdnyHe/gP3rmSTrihKFVuA3cZM62sDR4FcaeWH
            SnKSp02pqjBOjC/dOK97nXpKLJgNH046owIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICLuhLRmt8M5s2Edwwl9XY0KAAivzmPCEweesH5/KhR4";
    };
    schnabeldrucker = {
      nets = {
        gg23 = {
          addrs4 = ["10.23.1.21"];
          aliases = ["schnabeldrucker.gg23"];
        };
      };
    };
    schnabelscanner = {
      nets = {
        gg23 = {
          addrs4 = ["10.23.1.22"];
          aliases = ["schnabelscanner.gg23"];
        };
      };
    };
    wu = {
      cores = 4;
      # TODO wu is mobile, so dc means "home data center"
      dc = "tv"; #dc = "gg23";
      nets = {
        gg23 = {
          addrs4 = ["10.23.1.37"];
          aliases = ["wu.gg23"];
        };
        retiolum = {
          addrs4 = ["10.243.13.37"];
          addrs6 = ["42:0:0:0:0:0:0:1337"];
          aliases = [
            "wu.retiolum"
            "cgit.wu.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEArDvU0cuBsVqTjCX2TlWL4XHSy4qSjUhjrDvUPZSKTVN7x6OENCUn
            M27g9H7j4/Jw/8IHoJLiKnXHavOoc9UJM+P9Fla/4TTVADr69UDSnLgH+wGiHcEg
            GxPkb2jt0Z8zcpD6Fusj1ATs3sssaLHTHvg1D0LylEWA3cI4WPP13v23PkyUENQT
            KpSWfR+obqDl38Q7LuFi6dH9ruyvqK+4syddrBwjPXrcNxcGL9QbDn7+foRNiWw4
            4CE5z25oGG2iWMShI7fe3ji/fMUAl7DSOOrHVVG9eMtpzy+uI8veOHrdTax4oKik
            AFGCrMIov3F0GIeu3nDlrTIZPZDTodbFKQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcJvu8JDVzObLUtlAQg9qVugthKSfitwCljuJ5liyHa";
    };
    xu = {
      cores = 4;
      # TODO xu is mobile, so dc means "home data center"
      dc = "tv"; #dc = "gg23";
      nets = {
        gg23 = {
          addrs4 = ["10.23.1.38"];
          aliases = ["xu.gg23"];
        };
        retiolum = {
          addrs4 = ["10.243.13.38"];
          addrs6 = ["42:0:0:0:0:0:0:1338"];
          aliases = [
            "xu.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAl3l7IWbfbkVgaJFM3s9g2UCh2rmqoTba16Of7NNWMj05L/hIkUsQ
            uc43/QzidWh/4gEaq5MQ7JpLyzVBQYRJkNlPRF/Z07KdLBskAZCjDYdYue9BrziX
            8s2Irs2+FNbCK2LqtrPhbcXQJvixsk6vjl2OBpWTDUcDEsk+D1YQilxdtyUzCUkw
            mmRo/mzNsLZsYlSgZ6El/ZLkRdtexAzGxJ0DrukpDR0uqXXkp7jUaxRCZ+Cwanvj
            4I1Hu5aHzWB7KJ1SIvpX3a4f+mun1gh3TPqWP5PUqJok1PSuScz6P2UGaLZZyH63
            4o+9nGJPuzb9bpMVRaVGtKXd39jwY7mbqwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPnjfceKuHNQu7S4eYFN1FqgzMqiL7haNZMh2ZLhvuhK root@xu";
    };
  };
  users = addNames rec {
    mv = {
      mail = "mv@cd.retiolum";
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGer9e2+Lew7vnisgBbsFNECEIkpNJgEaqQqgb9inWkQ mv@vod";
    };
    tv = {
      mail = "tv@wu.retiolum";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEAQDFR//RnCvEZAt0F6ExDsatKZ/DDdifanuSL360mqOhaFieKI34RoOwfQT9T+Ga52Vh5V2La6esvlph686EdgzeKLvDoxEwFM9ZYFBcMrNzu4bMTlgE7YUYw5JiORyXNfznBGnme6qpuvx9ibYhUyiZo99kM8ys5YrUHrP2JXQJMezDFZHxT4GFMOuSdh/1daGoKKD6hYL/jEHX8CI4E3BSmKK6ygYr1fVX0K0Tv77lIi5mLXucjR7CytWYWYnhM6DC3Hxpv2zRkPgf3k0x/Y1hrw3V/r0Me5h90pd2C8pFaWA2ZoUT/fmyVqvx1tZPYToU/O2dMItY0zgx2kR0yD+6g7Aahz3R+KlXkV8k5c8bbTbfGnZWDR1ZlbLRM9Yt5vosfwapUD90MmVkpmR3wUkO2sUKi80QfC7b4KvSDXQ+MImbGxMaU5Bnsq1PqLN95q+uat3nlAVBAELkcx51FlE9CaIS65y4J7FEDg8BE5JeuCNshh62VSYRXVSFt8bk3f/TFGgzC8OIo14BhVmiRQQ503Z1sROyf5xLX2a/EJavMm1i2Bs2TH6ROKY9z5Pz8hT5US0r381V8oG7TZyLF9HTtoy3wCYsgWA5EmLanjAsVU2YEeAA0rxzdtYP8Y2okFiJ6u+M4HQZ3Wg3peSodyp3vxdYce2vk4EKeqEFuuS82850DYb7Et7fmp+wQQUT8Q/bMO0DreWjHoMM5lE4LJ4ME6AxksmMiFtfo/4Fe2q9D+LAqZ+ANOcv9M+8Rn6ngiYmuRNd0l/a02q1PEvO6vTfXgcl4f7Z1IULHPEaDNZHCJS1K5RXYFqYQ6OHsTmOm7hnwaRAS97+VFMo1i5uvTx9nYaAcY7yzq3Ckfb67dMBKApGOpJpkvPgfrP7bgBO5rOZXM1opXqVPb09nljAhhAhyCTh1e/8+mJrBo0cLQ/LupQzVxGDgm3awSMPxsZAN45PSWz76zzxdDa1MMo51do+VJHfs7Wl0NcXAQrniOBYL9Wqt0qNkn1gY5smkkISGeQ/vxNap4MmzeZE7b5fpOy+2fpcRVQLpc4nooQzJvSVTFz+25lgZ6iHf45K87gQFMIAri1Pf/EDDpL87az+bRWvWi+BA2kMe1kf+Ay1LyMz8r+g51H0ma0bNFh6+fbWMfUiD9JCepIObclnUJ4NlWfcgHxTf17d/4tl6z4DTcLpCCk8Da77JouSHgvtcRbRlFV1OfhWZLXUsrlfpaQTiItv6TGIr3k7+7b66o3Qw/GQVs5GmYifaIZIz8n8my4XjkaMBd0SZfBzzvFjHMq6YUP9+SbjvReqofuoO+5tW1wTYZXitFFBfwuHlXm6w77K5QDBW6olT7pat41/F5eGxLcz tv@wu";
    };
    tv_nomic = {
      inherit (tv) mail;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3dYR/n4Yw8OsYmfR2rSUG7o10G6AqOlSJHuHSEmANbMkqjiWl1TnpORtt5bAktyAGI4vWf9vhNEnxuIqGXWSV+3yCd7yfBHR1m0Y9QSw6blQ0xc1venl3JU0kpEyJfUn8a9cdXlnRiS0MP1gcsN7Zk8cqBELJYJajkSEnsT4eVaU5/wdnyzUO1fk8D8tFBJbF/tsWDLJPu4P18rpxq4wZgA2qmyHoVDEVlrz2OYcziXT6gpG0JGnToteaNg9ok5QavEYFpp8P+k1AacrBjc1PAb4MaMX1nfkSyaZwSqLdH35XkNRgPhVVmqZ5PlG3VeNpPSwpdcKi8P3zH1xG9g6Usx1SAyvcoAyGHdOwmFuA2tc1HgYEiQ+OsPrHZHujBOOZsKTN9+IZHScCAe+UmUcK413WEZKPs8PeFjf1gQAoDXb55JpksxLAnC/SQOl4FhkctIAXxr12ALlyt9UFPzIoj/Nj2MpFzGSlf653fTFmnMbQ8+GICc4TUpqx5GELZhfQuprBTv/55a9zKvM4B8XT3Bn9olQzMQIXEjXb3WUVFDDNWeNydToorYn1wG3ZWQ+3f0IlqRicWO7Q9BRj1Lp5rcUCb+naJ48tGY6HFUZ1Kz/0x458GDFvUd8mCJjqqmeSkUEeZd0xet5tVFg/bYoSslEqPF6pz7V3ruJMSdYxnQ== tv@nomic #2";
    };
    tv_xu = {
      inherit (tv) mail;
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC/3nkqxe8YrDVt615n96A7iC3vvwsiqgpsBYC/bhwfBHu1bAtBmTWVqSKDIdwg7p8TQpIKtAgZ3IJT3BlrnVTeR4RIviLjHjYWW1NBhm+nXi+heThgi5fLciE3lVLVsy5X9Kc1ZPLgLa1In0REOanwbueOD0ESN1yKIDwUUdczw/o3dLDMzanqFHKuSSN4o9Ex2x+MRj9eLsb706s4VSYMo3lirRCJeAOGv1C7Xg1cuepdhIeJsq9aF7vSy15c0nCkWwr8zdY7pbMPYCe5zvIEymZ0UowZ5HQ3NmIZnYDxa4E1PFjDczHdQbVmmGMI80grNwMsHzQ6bynHSPXDoLf4WodXlhS0+9Ju5QavDT6uqZ9uhDBuWC8QNgWUMIJnEaTBFyA0OI1akl8Q2RLC+qnNf5IwItSq+GDwEsB2ZJNW3kOk1kNiCUrBafRYpPaFeP97wzzP4uYlBKAr2SOLrrkf7NFEdw2ihxhDMNnps/ErRJ8U0zdpmalw8mItGyqRULpHjk/wN00rYOdBIhW3G3QJuVgtGnWtGCBG5x70EfMiSEXPD3YSsVVsgKD+v8qr+YiilRRD+N3gaHhiOWA6HgxRNul/P4llk0ktTpb9LoHk2+oooTH5ZuuT/8yF8J4stZt7EIOH+mSOAXG1z0BwnEkQu7pVKwu/oOZpGJTvBrGwww== tv@xu";
    };
  };
}
