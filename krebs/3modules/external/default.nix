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
    kabsa = {
      owner = config.krebs.users.kmein;
      nets = {
        retiolum = {
          ip4.addr = "10.243.2.4";
          aliases = [
            "kabsa.r"
            "kabsa.kmein.r"
          ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAtz/MY5OSxJqrEMv6Iwjk
            g/V58MATljj+2bmOuOuPui/AUYHEZX759lHW4MgLjYdNbZEoVq8UgkxNk0KPGlSg
            2lsJ7FneCU7jBSE2iLT1aHuNFFa56KzSThFUl6Nj6Vyg5ghSmDF2tikurtG2q+Ay
            uxf5/yEhFUPc1ZxmvJDqVHMeW5RZkuKXH00C7yN+gdcPuuFEFq+OtHNkBVmaxu7L
            a8Q6b/QbrwQJAR9FAcm5WSQIj2brv50qnD8pZrU4loVu8dseQIicWkRowC0bzjAo
            IHZTbF/S+CK0u0/q395sWRQJISkD+WAZKz5qOGHc4djJHBR3PWgHWBnRdkYqlQYM
            C9zA/n4I+Y2BEfTWtgkD2g0dDssNGP5dlgFScGmRclR9pJ/7dsIbIeo9C72c6q3q
            sg0EIWggQ8xyWrUTXIMoDXt37htlTSnTgjGsuwRzjotAEMJmgynWRf3br3yYChrq
            10Exq8Lej+iOuKbdAXlwjKEk0qwN7JWft3OzVc2DMtKf7rcZQkBoLfWKzaCTQ4xo
            1Y7d4OlcjbgrkLwHltTaShyosm8kbttdeinyBG1xqQcK11pMO43GFj8om+uKrz57
            lQUVipu6H3WIVGnvLmr0e9MQfThpC1em/7Aq2exn1JNUHhCdEho/mK2x/doiiI+0
            QAD64zPmuo9wsHnSMR2oKs0CAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
    };
    helsinki = {
      owner = config.krebs.users.ajs124;
      nets = {
        retiolum = {
          ip4.addr = "10.243.10.1";
          aliases = [ "helsinki.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA5MnCmT9xVEtv3hoZsjmgF4pVnPgzaWWVrZVguMfXcbTiusgWqBIM
            Ms/Ue676J3kQIJT1QSMA2RWDUU++dUcfhHF87vFpnyCnaKjfz6LyAwlSfKluttyY
            aFNgcUWlZRl4wkqys/oYhLD1q26mO/ekCA2eohzbB1TCaPY97VM5nl0MkXStMN76
            B+Ipw/gQcZXuWYct8Nj69sETPLnbf6ZBKs+T213as+NUSHVUdmBmV8QtmHDI3e7B
            4wAK1JkFCQgVu9gm/6BvqWroOMcmtxzSceyrY+0MWnAnM+wrLcYYaT2xw1OJyWmM
            riOHHMw9iLVxqyQ+3eDucJRQcJzO7I4j8zQaoYxPE1ZSl4wTsaypkMuNOyhYv2C9
            RNPJgTBlr911NnY7TcKauz/lO1Qcl5kHBMrIfwsbsQK+zfN2XX+s21/SPeSJ7k1N
            OqXeyX0mz2l7bhBDwTTDtINSz8sB3BL59mVbWY5z5b90oeKPrfygmp7V0CSKgHBr
            b5ZIhMRfgcK+HjolcEqdL9INpJZVFYt3vWPNhDpbX5sEOjjR+ODceriL8zdlTBRx
            PyB9OiK6tN+L63QFM7H1NFN9fPeOd2WbHvfoeX255kx8FHSALKL5rVSz9Ejwc97k
            rG0FItgHXajPazulBfUV0N9ck7SwLTmStKxtQ8NKCoIJLpv2ip4C+t0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = 47fX1g6qynVprA+PtniBLEonFp1B70nMrJ8SBCWNJnL
          '';
        };
      };
    };
    horisa = {
      cores = 2;
      owner = config.krebs.users.ulrich; # main laptop
      nets = {
        retiolum = {
          ip4.addr = "10.243.226.213";
          ip6.addr = "42:0:e644:9099:4f8:b9aa:3856:4e85";
          aliases = [
            "horisa.r"
          ];
          tinc.pubkey = tinc-for "horisa";
        };
      };
    };
    hasegateway = {
      cores = 1;
      owner = config.krebs.users.hase;
      nets = {
        #internet = {
        #  ip.addr = "37.24.200.174";
        #  aliases = [ "hasegateway.i" ];
        #};
        retiolum = {
          ip4.addr = "10.243.226.216";
          ip6.addr = "42:0:e644:9099:4f8:b9aa:3856:4e86";
          aliases = [
            "hasegateway.r"
          ];
          tinc.pubkey = tinc-for "hasegateway";
        };
      };
    };
    jongepad = {
      owner = config.krebs.users.jonge;
      nets = {
        retiolum = {
          ip4.addr = "10.243.5.6";
          aliases = [
            "jongepad.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAtJsF5jL/M72PCptLFC5iIEt0qAL544H/VLijvZEG9gnoqbs94aNJ
            MM5Sr3yMB01WkcT1Lph3r4dxV0/QECu3Ca4xxuUntu42tFXhkikQGcZLuo2h4zr4
            +wReudCCc7VqMcJDxriyyoW3i7smZnQGzo36gpKHbZfil8dJo0QE8mnujqkQCA0G
            hjR7xdG+/usDgRUarfpNgoHKyZfLcomQLUuR8I3aHsdaCLgMJ8v5DjGymp2bIswT
            puPx3IEZSXH8y6MZoISvLn+hwcWat34Bj1PF7vfgldivqHaDFpifpXvjbCmxcel9
            WVZRSEvLSVT4FnpaJ7JkAaUpG+GOHVlPWARq9t9AZXKR1Zex9MIkHzWi/TIIkawj
            wJNvUwvBYJ1UCuCby4/3nKlY7zWjj23YM6dTJDGMhJKR5m2SHp9SC0m0QdfSjN5z
            8sJauCigGZ6rlmxkO4/2BBGshY8jWDl/z2oFiQfo7R2oZkJdWNHLGKtTZtqQQ3e6
            SAE/HQvipiv35rMzHw3E9AJBhhQqT3vTLLZvMTBS6BRFvpqDNhXik1aFenNV4tjZ
            XeYU1eXI4XzQqoW/avPTuLt8O0Ya/nziLXCaIy+hlx5Hd49hkGb+1saQ5yPUgoEt
            wE9sy5+9b5ebn8B+N0yw7wnUYN8V8dmPmRwLt71IuBwHn/aAoXyWwFsCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    justraute = {
      owner = config.krebs.users.raute; # laptop
      nets = {
        retiolum = {
          ip4.addr = "10.243.183.231";
          aliases = [
            "justraute.r"
          ];
          tinc.pubkey = tinc-for "justraute";
        };
      };
    };
    makanek = {
      owner = config.krebs.users.kmein;
      nets = {
        retiolum = {
          ip4.addr = "10.243.2.84";
          aliases = [
            "makanek.r"
            "makanek.kmein.r"
            "grafana.kmein.r"
            "names.kmein.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAwvtxCG7Vua6+WoStGrkL+H/g4BABidL2eikDBtbxWN+oGv2Bjrwb
            VzXB8lMTCsu6M2wb3YTXXzAkc5oI4gE1sSOiCKyhYnQRrndc91KewquxTPfKL19u
            JiRqax/E49IvWKARPRPXUhPfU/NNw1oIxhbcFkjwJmqDvh9SWhl5VZVynCE28ov5
            hjjhqNXZHOR8CQqPJeY8v38OAAwTWvJ6rhEQwp5dLBqmRAbvPXj7OOzCxKluDY2X
            Dl4k6QAjI6NddJXsXHRYRNGiB0CP1UBC91NDtW2+HIjf1Q1Lip5agO4/SkkSUt39
            de7uYKrNcfyDUBb9Suconw0HvW+Dv4Ce5um+aa1RUrWIQdqBCOECbsXYKp66vAnK
            Hidf2uznFseWxiyxz1ow8AvvSseABej5GuHI/56lywnFlnHEZLREUF/4PT+BZ0vE
            uPRkzqeSNybsYYFieK8aany/RmJaoIsduGutgAiKBvkCCHru895n02xuLhZVkM2G
            zfVjA2le+Gxr21/sVPrHfxResLUh4RA8fkS7erWMRF4a3IOpIS4yrM+p4pZTlTxO
            Ya8buh4RgyE/0hp4QNpa4K7fvntriK+k6zHs7BcZcG2aMWP3O9/4DgjzBR3eslQV
            oou23ajP11wyfrmZK0/PQGTpsU472Jj+06KtMAaH0zo4vAR8s2kV1ukCAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = s/HNXjzVyDiBZImQdhJqUmj7symv+po9D9uDj+/6c2F
          '';
        };
      };
    };
    manakish = {
      owner = config.krebs.users.kmein;
      nets = {
        retiolum = {
          ip4.addr = "10.243.2.85";
          aliases = [
            "manakish.r"
            "manakish.kmein.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAtZcWwm1tTFoMcO0EOwNdSrZW9m2tSNWzwTGjlfuNFQKPnHiKdFFH
            Hym72+WtaIZmffermGTfYdMoB/lWgOB0glqH9oSBFvrLVDgdQL2il589EXBd/1Qy
            7Ye5EVy2/xEA7iZGg3j0i+q1ic48tt6ePd4+QR0LmLEa8+Gz5X0Tp9TTf7gdv+lB
            dVA6p7LJixKcBsC5W0jY5oTGUP0fM844AtWbpflmlz0JZNWrkJhCksOnfhUzeIsF
            1m9rCsyK+3jGMV6ZxhEbwaOt99Wlv0N0ouPePw+xLnnGTu0rJ/RKWceYnWnrHIyb
            GgGIHnm9GbMd4mAfyp63emRYDMclSQSrddpDUL2GK8TCTttr6bZm4M/pFuXQGJsQ
            EG0iaE8FM+nCrhmCRnX8dRWcNmHybd34UoVGCDJ6u+ksLIivqgWeY41CauqN0vQw
            U4zqp6XMXRB6vlVcyLzdTASxVKaLJt+BuvHcyqz/YslJ97z4yoLE3d7s/9gZkM//
            +FD970bsyvKpKRx72rNRCO9tQJNgPsaMiW5nuHUFw71XxX8o0w//5a0h5cdbiT64
            I4ISySa4ynmHI1/v0a937/sFS0IvRI1Va0Efh2VxasNIqpDmM3hA8auPDj0Js/4c
            qVnWMbvqqYlY9l//HCNxUXIhi0vcOr2PoCxBtcP5pHY8nNphQrPjRrcCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    qubasa = {
      owner = config.krebs.users.qubasa;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.175";
          aliases = [ "qubasa.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA65g1Xql+S+Dd90uDpSVxzGRTL8n4DHc1p9T8u9h7ioytC9B+e2dQ
            RU/y3gdJ0gXxrbth36MhTANuUonnqpHvsWwUDCQRbxLEFh8avlzLsecWvwrIt3zL
            102EaVurRySUa83D6TK8ZsDa2+ADY7tEzfFMJhT53g7MpBNIeOquB0rR6hVYBbHc
            3B+QtwdM8dx1gO/5+FsPYhJbR7ARczYHsj7Eyb8NbdzthEO0ICDgwzmcXTJfVHGR
            qfT7DUolXsu7uSPMLB+Pe/leI7XcQ2VFukpVGP0fZv0mSMxavFlcFVkLgdbAEd2H
            DPEBEcJpLR4Hw3HlO1kPPufaUdoeNhUmTkIp76mkCbanS1P/aFNFFcVB+a/+tpdK
            z5pG8K3qANg5txp6sAatPchvkeQelIg11lvT9luc+nFsTEW6Ky5nDLo60luZVFnn
            i1bdVeOojXR0u7M2gMqQZcSuscvy8APe48S8vPsqoiob1l/r77B7iNrWDwH8IutW
            u8fpC64CbhlR76Orp3xTZPmJQCRT8XYpKDDoq5Z7prdlAEz3U6wEfVckVv+f1dmU
            odG0zDTsmyKhkWWmZbPgPrOEUvAVoSpSLSQQxPR+UHArlgYe+2dAf8IHYqrgmhuO
            D4Lga4nNwTyVbCZ8vUu5b/lnGCLpNcVj22WVQTdAJzNsCVTdIM2V5hcCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    rilke = {
      owner = config.krebs.users.kmein;
      nets.wiregrill = {
        aliases = [ "rilke.w" ];
        wireguard.pubkey = "09yVPHL/ucvqc6V5n7vFQ2Oi1LBMdwQZDL+7jBwy+iQ=";
      };
    };
    rtjure = {
      owner = config.krebs.users.rtjure;
      nets = {
        retiolum = {
          ip4.addr = "10.243.122.122";
          aliases = [
            "rtjure.r"
          ];
          tinc.pubkey = ''
             -----BEGIN RSA PUBLIC KEY-----
             MIICCgKCAgEA3YkPPsO3WDGrXyOBdAxxP1MNNuPa19Gx1pA73FKv0gnfp4wYyjwl
             sc9A0C5yr741+LhJNqfkUT9Vb7dE2PZcEcAxZ6Vk9FBkkCWHGVyMfeqeK/hTuYqk
             FKGNPcGWCKZDM6CYSNYr2PW3ER8xMrQP9VSvHk1smdqr8cj3wWJ8TRtUmHzkvPZc
             C4bgrLDiQ8uev5VCt4POilrnjfcBNzgOFxWZ5uneTwM6tLhOj9uaylJEtDbW2XrF
             ocm8cGrYkS4c1x77mz/eYfJUJQFhTVGp29QTIiIHglP7W67LLq4qMvREvRhGTovd
             AT4KUOEXRgcPzHhbcVNeu2/ekKGHAubpjFfqxW7Y9zRTOXeSwyDnVbh+jg/VBGIV
             2BQZnUqNSQIHVeHQCoI3ugdSsqK5Gf1z9cKqpeNfwo+JK72NTC+nH2d5ypRksTzv
             VoTrFrv0P2qtKkhI79zY3ezw3HjCf6osKz9/EAYgzGH1Ix4WD3jjc1gqePiHYYlL
             EQV4HkwmarmMNrNA8qRDhKCTK4G7CS6btOcSsCM3y1lYbkubaOncIACSWIJ1uAMJ
             SEY30YYtOw2PPWstaWdy8MMZK8/MAXGEkt10OBpai7AdFZq8Oyz6xmLpgVIsWPbt
             UI8BvkKmFhMU2EHKUbe0qe5M1r218dsrOjPk99QI99iazMG34hyxQB8CAwEAAQ==
             -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    zaatar = {
      owner = config.krebs.users.kmein;
      nets = {
        retiolum = {
          ip4.addr = "10.243.2.34";
          aliases = [
            "zaatar.r"
            "zaatar.kmein.r"
            "radio.kmein.r"
            "bvg.kmein.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAyEeesa4mDNAT2QT/PxfmildhqawinWTcUiF3V2wsfy3eXJCFySKM
            pRKrLnOpkd2RoLWA88o0/hAfTdel/fXRjAx8TtKlh1n+UoRhYOGqLJKUZDVGImjJ
            xTPGCC+g4b2cNCYU3RVdttSu8+zlfO6ktkySULKbVkJN3ocQmSCmWs2tP6hYvk2i
            5OB3Uxw+OwhtVO76dlby6Idmc8o++ZVm3snbYsgiR7RQf9vHZHR8lKr5fZ/ycq2Q
            T+agY/cFLJ+jhfUi8LFtKKcqGLyKKrDywADQWTcIG+cjV33k6ftW678jvsEft6e6
            2RgspZX5XciTbMPIPGMcH5NZPXi6youcUcqcqDtok7Y1Jj3N5dSmJno5ojyKolZp
            PRt4bPx9WuerjzwL5gG9E6n6l24neX6Mq7RDyJfBYtpUvZblezXWUuoReGNczAvj
            zZrAKXKnuCEgQ/du7pT8V6qHG5NjovIMm0VDxOJV5jBL4NUox3PGbW5g0vS/nxHc
            xKWPq+4zoyA6MsL9sGCqIlSWEqNnSERX19GbJZNYjm1J+aGZ/fZ+MaDJGuCzlxn4
            yjLBuuXSkIrPxxtIV+Yh8Wy5qDNRN7XS1wNxUcmjQn0+7Ou/4W+cTWJ/+yZyC1DK
            uYEZh8YBMJo0E4bR4s04SFA6uLIvLigPELxzb0jwZSKXRnQhay6zzZ0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    sokrateslaptop = {
      owner = config.krebs.users.sokratess;
      nets = {
        retiolum = {
          ip4.addr = "10.243.142.104";
          aliases = [
            "sokrateslaptop.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA0EMbBv5NCSns4V/VR/NJHhwe2qNLUYjWWtCDY4zDuoiJdm3JNZJ2
            t0iKNxFwd6Mmg3ahAlndsH4FOjOBGBQCgBG25VRnQgli1sypI/gYTsSgIWHVIRoZ
            rgrng0K3oyJ6FuTP+nH1rd7UAYkrOQolXQBY+LqAbxOVjiJl+DpbAXIxCIs5TBeW
            egtBiXZ1S53Lv5EGFXug716XlgZLHjw7PzRLJXSlvUAIRZj0Sjq4UD9VrhazM9s5
            aDuxJIdknccEEXm6NK7a51hU/o8L+T0IUpZxhaXOdi6fvO/y3TbffKb1yRTbN0/V
            VBjBh18Le7h0SmAEED5tz7NOCrAjMZQtJQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    toastbrot = {
      owner = config.krebs.users.jan;
      nets = {
        retiolum = {
          ip4.addr = "10.243.117.12";
          aliases = [
            "toastbrot.r"
          ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA12VLPJMhGSh5fQgrB6bP
            2H1eew0/7t1xr3oJ3uaTZd7UIvPQ/aA5pB9xL5s+BIBvRa5P3QFWUAVhqchsPiRc
            yC4awLvo6zrUZB3pJBFiUuThx1xzmazTbRNyJ0E3Dwi2VSp3dAi5xEwHSVDSElGj
            DyRrdwyLe9lKghGHgNhB01QAt1/AO3A/TBs2RS/E0kuPhVQzpo5Ae5I530Cr0pf3
            r/de1TdArIcOfnTvW7WNrdBhwLq14cfdXkZwJ2bBE9Q22FAJp5k21PW5dQ41oDuT
            PYHZIH555sxifMThrUpuNHIrDtIQk6D+Km90WNf/lBGwZqQr/B5G6zSNX7d/0JbY
            Hi8Ltq++Sf0XgWNir9+evGNLCBqAXdvQFrj2l7BuNywE0L2nZThnxjTxP6QLFnqO
            IXY97x3p7AYcfmVFutfYqYM1HdyyehF711hhm30fdcXHsJ+GpQgGrj67+++N7g7g
            fjWBGNI9EL9CyTZ/N9U3TGeoxooc1BSaAiHmaPoYaAeI0Y/W6bNrixpL3aI5X8MH
            Flen2y2XEk2n+pXozPDbLAT+MZ3sWwODDYRc8zGbV2RlMvL94LHh95/JC0itdXa3
            uNRDtSnfbNe4eHw9/HMDkclhywuE+hbyq+JNNodqLwG/o1/r3GI+ggOyCdZHjF4B
            4R8QXUJiqUdcbR3WQDR5i10CAwEAAQ==
            -----END PUBLIC KEY-----
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
          aliases = [
            "tpsw.r"
          ];
          tinc.pubkey = tinc-for "tpsw";
        };
      };
    };
    uppreisn = {
      owner = config.krebs.users.ilmu;
      nets = {
        retiolum = {
          ip4.addr = "10.243.42.13";
          aliases = [ "ilmu.r" ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAweAz7KtgYVuAfqP7Zoax
            BrQ++qig30Aabnou5C62bYIf1Fn8Z9RbDROTmkGeF7No7mZ7wH0hNpRXo1N/sLNt
            gr4bX7fXAvQ3NeeoMmM6VcC+pExnE4NMMnu0Dm3Z/WcQkCsJukkcvpC1gWkjPXea
            gn3ODl2wbKMiRBhQDA2Ro0zDQ+gAIsgtS9fDA85Rb0AToLwifHHavz81SXF+9piv
            qIl3rJZVBo1kOiolv5BCh4/O+R5boiFfPGAiqEcob0cTcmSCXaMqis8UNorlm08j
            ytNG7kazeRQb9olJ/ovCA1b+6iAZ4251twuQkHfNdfC3VM32jbGq7skMyhX3qN/b
            WoHHeBZR8eH5MpTTIODI+r4cLswAJqlCk816bGMmg6MuZutTlQCRTy1S/wXY/8ei
            STAZ1IZH6dnwCJ9HXgMC6hcYuOs/KmvSdaa7F+yTEq83IAASewbRgn/YHsMksftI
            d8db17rEOT5uC1jOGKF98d7e30MX5saTJZLB6XmNDsql/lFoooGzTz/L80JUYiJ0
            fQFADznZpA+NE+teOH9aXsucDQkX6BOPSO4XKXV86RIejHUSEx5WdaqGOUfmhFUo
            9hZhr0qiiKNlXlP8noM9n+hPNKNkOlctQcpnatgdU3uQMtITPyKSLMUDoQIJlSgq
            lak5LCqzwU9qa9EQSU4nLZ0CAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
    };
    crustacea = {
      owner = config.krebs.users."0x4A6F";
      nets = {
        retiolum = {
          ip4.addr = "10.243.42.63";
          ip6.addr = "42:4a6f::4263";
          aliases = [
            "crustacea.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA1dA67Uq6IcWTWVVcg5kO4OUcmYY/mUzERK6WwrU7m+Qq2ovA2Fh1
            VTxfNzJg8zgyrBbUwpaLE4LuRgyrYbPABwgNMXS6wnHdunbm0x5RUcih/IRNobV1
            uf2Q/rVcrXHZD5+YL09hTZnU7PVkZm6WX0fc79rEKYIEopPpomCs2mECPSmqZPaW
            L9wprtRTuQ3V0xxrCuUKX7SxANEursM8SvNfKydWdaUxjIV2iGVKuVUkAZHwx5jJ
            roKEriWsIJ6FHTMb1v5yWXrvngMgYlKrpF6/m/AHEkQoPsEJ+oBkn0fop9pfyZjM
            WzMhZHcKFYebSI4HqFRuQOc1scIzUdpC/sZYLYlddbwpJHj4xdJwIUN03Uga/KRQ
            n1SrJnhmXonHvJZFBYcNDR6aTtdN7mJVBv8bQ7DGt1q6Gp8QItQqvpdzq314+Pw6
            0EVKPaqdz6Cqpwn8RtJ9ZGb6BE3yUrpJkU25DyCSO86LmeCchApwssghWvPsbBDg
            iF4QCyrWJ2HFnl7jJDGbEajHaE/xko2dt1F5frTWxsmDHRKSRhaGDwp5qgFUpCa0
            2h+zZqkG4boV6CrMEjStb15EOXTUVfq0DPojFik6agCltslsJAwp+f1fb7NSee4d
            TNWb1CHfIQWLPnm1LFwphSqyHY/9ehcsX3PJ7oXI+/BnV8ivvoApWA0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = DWfh6H8Qco+GURdVRhKhLBAsN5epsEYhOM2+88dTdTE
          '';
        };
      };
    };
    unnamed = {
      owner = config.krebs.users.pie_;
      nets = {
        retiolum = {
          ip4.addr = "10.243.3.14";
          aliases = [ "unnamed.r" ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAvGXVl+WV/bDxFAnYnAhZ
            2rHCU5dqtBvSg0sywV1j++lEuELBx4Zq14qyjDRGkkIGdgzCZBLK2cCgxPJ3MRFx
            ZwiO3jPscTu3I7zju7ULO/LqGQG+Yf86estfGh394zFJ2rnFSwegeMNqCpOaurOH
            GuYtNdjkxn/2wj00s+JEJjCNRMg8bkTMT3czuTr2k+6ICI8SgLZMDH7TjRfePHEW
            X9/v4O3kMSZccT/wZWmezXuYlO7CJs7f4VV98z+sgubmIZz3uLfQFY8y9gmGp46y
            5n5QyD0iIqkLNGIldNnToVJPToRaW5OdNKtZFayU4pWZ296sEcJI0NWLYqy7yZfD
            PG2FlCQmebUxMYk+iK0cYRLFzOgnr14uXihXxhuHYJ8R1VIbWuto1YFGUv5J/Jct
            3vgjwOlHwZKC9FTqnRjgp58QtnKneXGNZ446eKHUCmSRDKl8fc/m9ePHrISnGROY
            gXMieAmOZtsQIxwRpBGCLjrr3sx8RRNY8ROycqPaQWp3upp61jAvvQW3SIvkp1+M
            jGvfebJOSkEZurwGcWUar9w9t/oDfsV+R9Nm9n2IkdkNlnvXD1rcj7KqbFPtGf1a
            MmB3AmwyIVv9Rk1Vpjkz4EtL4kPqiuhPrf1bHQhAdcwqwFGyo8HXsoMedb3Irhwm
            OxwCRYLtEweku7HLhUVTnDkCAwEAAQ==
            -----END PUBLIC KEY-----
            '';
        };
      };
    };
    miaoski = {
      owner = config.krebs.users.miaoski;
      nets = {
        wiregrill = {
          aliases = [ "miaoski.w" ];
          wireguard = {
            pubkey = "8haz9JX5nAMORzNy89VdHC1Z9XA94ogaZsY3d2Rfkl4=";
          };
        };
      };
    };
    nxbg = {
      owner = config.krebs.users.rtjure;
      nets = {
        retiolum = {
          ip4.addr = "10.243.122.123";
          aliases = [
            "nxbg.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEArnkpu+oD59Shu9xcppkcelMT/uHsKvMKdudr67WZG/4F0xhd/5ex
            an8v3OWClztIsnB+5uwl0dgamfKDAfIdg5ll2ZHcXo7dAdAN7q8DkegJD9k/Pmmi
            YGsEwyENhAcX0/L1xHD5rGqH+6qQ/HrXPKPquoWCIlDDX41dyZQxJCTzkKlRGWhl
            uwEMHkR4sfGgsD/OFmTVHMqygWbQIBIrUKXcHahsVj5k0LZW+ejVAQwNlzuKZi2B
            n4maa6R0s4kRk2N8TPW11BcCl+rZlaX7RSn8vi+lA0Aq+A5SL1kXaKkKQT/9j2+n
            G/uCDpQ6ruXaNycDkemqZg/MHDanbm5SUtjqZarfT6tRr3bwvpndxeGCeOZHkehw
            iDiRsXszdwVDziRBlHs4WvFHTZUBLBsetOeo/LaB3Lt069nF5Cs6SZDi3z17ZMow
            5IU66KLQBDnSHqJWvAkBZsWrlZHMr3Csefaqli+qGpPr2gVgiwh3BrH8Ie1DBWJM
            ysY0XK98s3jhLfWtc8Fg99H7QYenrh9IwfiIr5kRTmYxLBoGHO7GBRovuziJYzj+
            G1D160xnRSqVdbIg9Az9OMBHfv9/HwYwwLpQ/154SRTY6f4H3iFMt+0lJwSS6nIl
            yN7HY7PKXieun8OsS3GhV/+r8UVcRmVk+who3x8Hw8MQJHp8lUNGjLUCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    nxrm = {
      owner = config.krebs.users.rtjure;
      nets = {
        retiolum = {
          ip4.addr = "10.243.122.124";
          aliases = [
            "nxrm.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAxPg9J+cpmazp8ZH2eCQwbq6GdU22Nhd/ySm+K/aN+x55C4QN6gMM
            cBW2o0nfHi4JtvqDtdw0s9pGh0GsLHHoQlFD/lGr1oCMAe0FeN4cSAwbUH1DYFPw
            KsyiXpXLVYCqt42JjzCM8HNUMBNDlnZ60z2Ashxj79PbYJ+i3oPEIE//Vf6MPOta
            vaDUXCbqsWKKEqG8t+rM4WRrqzVVpASq6Avs2x+eijVe0Yeq4tkHcO0z3SrV2TM1
            nAPYDL0QlHHBVtAt0tAfo4CC+HAwZJz8yZ0sWPzz/fJj/K3HwuFDBKZSrsIgSPBc
            +JCFefuI3aNc1fKTYIu0XqCqgdB0Xu2g/AkJcqXSvJQaNPFuyk5n79C2INHcpLrp
            s8NWwaUAH7XhNUGYnzevan3hiuSgIsT0T2cfERmEGyMn90fioYWN7TW9txfEX9qL
            I4mkmh1xqt8ipdpfGxYmUAAj9KoHEhAnDElblIXRWY3KLdY6gT4sO80K+hTbK/J+
            oyhU0nYcAnrFJNlSNjNucM/4UlCXqs4TaCM9cRggT6PmHy+M7vLebI4JGoOpCuYw
            W1fiyXCrzlTP0vidDtv9mr0vTTK78Nc8oGc46Yu3K1kFSQYS/pRCjnOin35sYe/K
            ahpclNJjom6tHxcwTriT0w6Yh/fCei7WCqpWtK2m4Qho/+WA3rFc3WUCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    nxdc = {
      owner = config.krebs.users.rtjure;
      nets = {
        retiolum = {
          ip4.addr = "10.243.122.125";
          aliases = [
            "nxdc.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA2mKpvIpNOlX7adMUQvcExJfPMf7oZggqJBL43nYSOzqK4b3rMUrY
            SgCYQp9YMf4CMbAuPe4nzAHt9fHqIwNmN8YRh29ku/5tPZDg+OK8zRPoapaeABU4
            nvW3IxWdGGxyoh9E5MkjHh0Q8Hwt220ZSV5XhlySNOm0cbYRmUqoUtekN+BAzHzA
            nl9Ew0iCfeu06XlIn/z1vCFxv8Vk5M0dFk87JP0Gpt0JOgVbATPPnsZUNqmrNpQv
            IDroHUSRQRQmMYs1rlDGC+06QIHOj1WTNxbbntMAPEn9WLaptnSpznJQAvX6hbc/
            zBwJE4eWkwgpgQcBlqbTE0Zp0T9mU4cpED5dh7X+DU3pDh3T3Tfr+9wIOmNBEaxq
            a6dQ5kkOTtAIsQ4WIazNMKTJF/abGqjvqJLTAbQjX24ZgpMwH05vodh2Y1KJB2pu
            XHFqlMgIBnG4lZuS5ZfidLb2b6pl02dG3wTijHrjZFjBQyYCcpcsGoq1iIErae7T
            HFsjPH+wOKnj1UOxcArl1cubC3QFNHs9bvAFqGzm3u8N4rV71/DTi++Ph3kZ7ed0
            fR1tW6GVv0icL2rhaATBVgGaMiglaeyRcbiEpkYD0VC499xgPF3JFDxnmBR1g1jZ
            5UtQV1h8QpBUWkRH/rU1OwhZ/KNcN3Mx5mhs5MSl/Bn/Kwe2URS6eKsCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    nxnx = {
      owner = config.krebs.users.rtjure;
      nets = {
        retiolum = {
          ip4.addr = "10.243.122.126";
          aliases = [
            "nxnx.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA2JWNe54YaFM+flK3LlPwgOSgVRmZi+e+Qhc6uJYIxkQcAvJKpKJQ
            1M4h7OE7eiJLdDp/aGaHe4BuII15/0lFJwYf1Zt8E1zN54QtwuELkDgOhgkhgvVb
            tO+maHh10xsQMFlhpUztEk8oQuBu5toC795nKY7lBR2o6V2dPbbVo1+qr7qArOWo
            cBlshRhEDjuzJUMHLlUGu43/miWeDewAq4O7U/nNNEz/v8KbESqP9HtTjelAeWz6
            zGha8hSn+Snkt76kP15drgn1L8MMFvnm5EeJ5VkehnpOi8Vi9Yqln+VGwlvbhEdK
            ST0gxNBKoSvLITS1P/ypfiEXARUOffgq+kLA2Hyet0DfBjCMD+WkTBlj1QyXLs10
            3/xBntlOQqBcLIdpi/yRs7miyQlyblqsyiQOCukIvibdHB1RLdVBhUE3A7hgw4R+
            +3ug/mQR+fDOpNB/sOkorcTVgA04KENUHc+6OqA0dvoAYr8l7N4+az3AtyHDNr5x
            4otjxOq4fmu80sbm5Ry9SoNYMc4fOuWIZDHZ/ntDKqzHw3BaNB9vNkpKj22nArI4
            cwAMPPJMJJ+Ef7tIzZ+NKtPudqztoLa5AYNllV7K9gS6NG0Yzk6iIQ42bKgfsZFn
            9AkCdv8EycNIAIbBomPv2XIKYlKs3RfWEjRcSl3TQl4b3bilCicgnLECAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    nxnv = {
      owner = config.krebs.users.rtjure;
      nets = {
        retiolum = {
          ip4.addr = "10.243.122.127";
          aliases = [
            "nxnv.r"
          ];
          tinc.pubkey = ''
           -----BEGIN RSA PUBLIC KEY-----
           MIICCgKCAgEAxEs92W/wRl3wlB6fNS2KUS+ubFAPLkgQYhk4JXeEeTpUq1H27oxB
           ZWgWOlLMqnvn3w+aHQviWWPl5F6jXCxDOWCwyLhZU4cs45+ub9KKezCeE8IN+gAt
           NKDqmRFzao9EXoT7sR65BblqEUR/Aqpykv7n4JdL5pGDbw1GGJ6Xf5QZo2sYm4wp
           wdqOROn/V2Sm8NgmD1K6Sa2i6BLHSvHqunI4qoTyMfGXl8sbw6I2iclpQy8td9bt
           1WA7F9kVTZdhaWgfpiZ8sKQ9LoFKoy6jnoppQcl/E8V2XNnjPy8obaLX9rTJ/deT
           eW9qmfZeYiFSaDLLWEIZjhaU2l9z72oWyUW8w8GZQD+ypGi+UDMkbAhRHiaVGOZy
           S7AodiEL2Ebzj6XJaNYC3LYm5R8U6XlvcHwn4FDtgKkqwXz08cZsPwQLoBjXUEi/
           9/A5WEwrmp62TJ/ZRcRwV8/dBklrc/4FT0q0CiMuCWcbjF891d68TvcXlVU3gCwN
           ld80CS17o2dOsBBW4nft7+9tL545p7mMjw6Oa4kRUTo2n1mYkMdTGZR+tOCD6hvW
           45IG7vGq5EnRwolekGoMRf8RthajU2RXcIoNWnVon0so0Rja+AU9G7dobd/2qila
           jta1Mou2vzUSAbdwXtBwJHlV9882p1utMlU9XVEZwQXfWSt488tQqzsCAwEAAQ==
           -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    ada = {
      owner = config.krebs.users.filly;
      nets = {
        wiregrill = {
          aliases = [ "ada.w" ];
          wireguard = {
            pubkey = "+t0j9j7TZqvSFPzgunnON/ArXVGpMS/L3DldpanLoUk=";
          };
        };
      };
    };
    domsen-backup = {
      owner = config.krebs.users.domsen;
      ci = false;
      external = true;
      syncthing.id = "22NLFY5-QMRM3BH-76QIBYI-OPMKVGM-DU4FNZI-3KN2POF-V4WIC6M-2SFFUAC";
      nets = {};
    };
    catalonia = {
     owner = config.krebs.users.xkey;
     nets = {
       retiolum = {
         ip4.addr = "10.243.13.12";
         aliases = [ "catalonia.r" ];
         tinc.pubkey = ''
           -----BEGIN RSA PUBLIC KEY-----
           MIICCgKCAgEAug+nej8/spuRHdzcfBYAuzUVoiq4YufmJqXSshvgf4aqjeVEt91Y
           gT6iBN8IKnMjYk3bAS7MxmgiyVE17MQlaQi0RSYY47M8I9TvCYtWX/FcXuP9e6CA
           VcalDUNpy2qNB+yEE8gMa8vDA3smKk/iK47jTtpWoPtvejLK/SCi8RdlYjKlOErE
           Yl9mCniGD1WEYgdrjf6Nl7av6uuGYNibivIMkB2JyGwGGmzvP+oBFi2Cwarw8K2e
           FK2VGrAfkgiP5rTPACHseoeCsJtRLozgzYzmS5M9XhP5ZoPkbtR/pL5btCwoCTlZ
           HotmLVg4DezbPjNOBB9gtJF4UuzQjSPNY6K1VvvLOhDwXdyln82LuNcm9l+cy9y3
           mGeSvqOouBugDqie6OpkF0KrRwlGQVwzwtnDohGd/5f7TbiPf1QjC+JP/m4mxZl3
           zE0BCOct9b4hUc/CFto71CPlytSbTsMhfJAn8JxttGvsWIAj+dQ0iuLXfLDflWt6
           sImmnOo28YInvFx6pKoxTwcV1AVrPWn5TSePhZM50dmzs0exltOISFECDhpPabU3
           ZymRCze8fH9Z3SHxfxTlTZV7IaW2kpyyBe1KsWpM46gLPk5icX+Xc6mdGwbdGBpf
           vDZ+BoHCjq9FfQrAu1+E83yCYyu+3fWrLSgYyrqjg0gPcCcnb1g6hqECAwEAAQ==
           -----END RSA PUBLIC KEY-----
         '';
        };
      };
    };
    karakalpakstan = {
     owner = config.krebs.users.xkey;
     nets = {
       retiolum = {
         ip4.addr = "10.243.161.1";
         aliases = [ "karakalpakstan.r" ];
         tinc.pubkey = ''
           -----BEGIN RSA PUBLIC KEY-----
           MIICCgKCAgEA45kRCvWIaVteKQiz31AOjkEwHwOns/6SGXYzL5IswoEOT/i/8Ihl
           l+ydTMTE28zs1nQp8MUBEdsJF02U8aEjPCyyMtZflZ+uaUAeJ0zAWTcb4AwdSjp+
           RKApp+LmVNDyx3W6rIgK7WYLfKhge4nRAlnshpekzaS2j7ccKhZMBIqyntYDJb6K
           lE4poAgemMlE0apFV54d3ohWCZurfJ/K6BpsX7h+uwqFPOHi+pD7D/e2dHhSLXtS
           0cuFseQwqDF+xd5MAmApHO8w/BEdKWeU19TZmzkC5TlIO1HcknMq4Y8QkzCc5PXb
           5WeEdi1CyIGePldFv91LoHepsMV3nrIF7n6ZmdTuxj5GH0A0Zg0z4hrWJuXk64JM
           bTpe/rDXWOG0IK0HN4z14ySD8yafLTV4gvH9Mg6jUqyqGfLpIK+o/N7ZavOeVKq5
           3Hf9c246v1vhHjnbat5GyY79PmimEvxR51mOItpRoyJYfdSa3KrvUki0MboCiYAU
           GKBmEw2BR3eybnejHqvAFov30MkmkOTz3mV/UPKELqhGCQf6UJAKG0GoxGpK3m8k
           epNSAKUpj8B7+JM3Ybgl+CoAm/+qu7Ojp5j4Onn0kgB2yXryHJaNOdgraCXI2yzt
           /n/eHElmKWoMCXhkV/mee1Cl2Y74XKivM6ov3lLvIDRxdXl46PvBFVkCAwEAAQ==
           -----END RSA PUBLIC KEY-----
         '';
        };
      };
    };
    hydrogen = {
      owner = config.krebs.users.sandro;
      nets = rec {
        internet.addrs = [ "hydrogen.supersandro.de" ];
        retiolum = {
          via = internet;
          ip4.addr = "10.243.54.54";
          aliases = [ "hydrogen.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA6bnwRKS+oWNc+ExOnJdqGHR1bnCEE9zkCqyONK3+xLg8QQIz6SvZ
            Nt5cO9kdZw9rA+4Dshg5m0RSnaP2TVtiDxcyv3zAW4TSOTGaBRp9WZmwhcKyxiON
            hvTBirsC1Domorew6++qHoZxtmHMKaF/uPoFLBqSvw3RHVoGzEAhGW9/LqZVa9k2
            D8dDRg7jTqH2oiIHct33Up5+zeFbmCo5xyDzl+pcwHxldi8sAupvpHx4KxXH1zMX
            YQATS2Rp8b52bGEQ52UKAbSOpJqyt9/o5vb+z2JZ52N1dDrphWHGfIcJa8DVt2VO
            n/V0kWuUhDh3Wwt7aIFcY0bUq4OurcVQQ5eMgwGbOEthdjLf1ou2gUJhf0zAeVQp
            IiaqeHTNn1+mtxBjl4+v+b0H/lhZPSgO74Jo28fAse+/itjM3kgkIKV0ldD5tWpv
            MHaKMB5Ui6swHZDV6nUxf5vlKPAM71/14cy1e/0ANFo5JvE66jWn7m5wn60HYwpB
            XnTOgIxue2rJ4F+Xtm8CTgS5TcV1AKR0cnmlU7WfVOVJHD2/4QTRYYTCR53/RoVq
            1T6DILDF71H92PNylujKSPA0CKI160xJ61Xy/T6MYl5u0+RblAgYr77o5HJwmXCe
            jFrCu3SKUIlJWYHWE8yNoR+VVYeXakbDFYE3KpVyBDG+ljUbia+Oel8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = 3IKIoZqg0jm9+pOOka2FEtihx0y8qAdJqKTuRfJtMpK
          '';
        };
      };
    };

  };
  users = {
    ajs124 = {
      mail = "stockholm@ajs124.de";
    };
    ciko = {
      mail = "wieczorek.stefan@googlemail.com";
    };
    exco = {
      mail = "dickbutt@excogitation.de";
      pubkey = ssh-for "exco";
    };
    ilmu = {
      mail = "ilmu@rishi.is";
    };
    jan = {
      mail = "jan.heidbrink@posteo.de";
    };
    jonge = {
      mail = "jacek.galowicz@gmail.com";
    };
    kmein = {
      mail = "kmein@posteo.de";
      pubkey = ssh-for "kmein";
    };
    mic92 = {
      mail = "joerg@thalheim.io";
      pubkey = ssh-for "mic92";
    };
    sandro = {};
    shannan = {
      mail = "shannan@lekwati.com";
      pubkey = ssh-for "shannan";
    };
    qubasa = {
      mail = "luis.nixos@gmail.com";
      pubkey = ssh-for "qubasa";
    };
    raute = {
      mail = "macxylo@gmail.com";
      pubkey = ssh-for "raute";
    };
    rtjure = {
      pubkey = ssh-for "rtjure";
    };
    sokratess = {
    };
    ulrich = {
      mail = "shackspace.de@myvdr.de";
      pubkey = ssh-for "ulrich";
    };
    hase = {
      mail = "hase.christian92@gmail.com";
      pubkey = ssh-for "hase";
    };
    neos = {
      mail = "neos@shackspace.de";
      pubkey = ssh-for "neos";
    };
    "0x4A6F" = {
      mail = "0x4a6f@shackspace.de";
      pubkey = ssh-for "0x4A6F";
    };
    xq = {
      mail = "xq@shackspace.de";
      pubkey = ssh-for "xq";
    };
    xkey = {};
    miaoski = {
    };
    filly = {
    };
    pie_ = {};
    domsen = {
    };
  };
}
