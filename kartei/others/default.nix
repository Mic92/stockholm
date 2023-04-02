with import ../../lib;
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
          '';
          tinc.pubkey_ed25519 = "47fX1g6qynVprA+PtniBLEonFp1B70nMrJ8SBCWNJnL";
        };
      };
    };
    horisa = {
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

    porree = {
      owner = config.krebs.users.pinpox;
      nets = {
        retiolum = {
          ip4.addr = "10.243.100.101";
          aliases = [ "porree.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAvUeG5/8O8ZyDulpvdKNcA20aZIUv3YdUe9XJ0lNUgMCg6YdWTmba
            03wfm/SPoUM4ZOb6/QyEmdNWxQbeKVPZ8cNs/uDLMmpEoSaSKWsp7ZqtrxxincUY
            AGhyrU0h59lIZXZtmIp1Uc3Y7GYdX9MM2RsOs+0dYwCzZlHjarmNjxLRBcEbugMR
            +86zrwtcLGKBmMMhRbMQ/y07zwxb5wFSF7nSgFD0yzFjmaBLU6mYtzRR0hb17n34
            xmB7Lpj7ROtbB20btTmdyXHuE8P+5mohJavqAnpKiLNFLt9tNk1Vc4qAxlDkubZW
            SoHuPNyE/563jooL1DHDQ2EZ4lDweTYlT0gAOYGCcNVoCFCs/rbFK4u2p5FCVAfE
            bhc0Fm3fKNk6An1wIsSEg7UcYeTbaFBHbqr+7TiLyS2aehkgy4FF4n6ACDeF9cHM
            80OHh+N1wz7uL7BHcniTws7VWyloPOoEUJlZEDzHF/p1nfpDULsqNM0o8QGNmOeT
            4J2Sp+jJLin/8g2tPH+zMSge5rnFlPuBaRVZ5dwajdyq3USNgtVH1lZFx8sTLsAC
            JEW9Jn3xPDCaLGWp2fFzsg0hPxOG0SYD75coZcfikJ6MedvHFHAmf9KBYJxFWcXa
            JGky/WwdAF2f34e2vk/mWrjfgBIDzXU01Opm99lgWuMr5HMkCzH1kssCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "pXVWAPo/1JWhRs/0ip4Hz0bOSHTJa7FfaI74elU8XsL";
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
            MIICCgKCAgEAwEaIkC/JxEI6mAnA2lnoNYRSVAVOggtm7XBAX2tTq9OCnwgh6Nnr
            Bv8S6j8HBybMqZHKBlfFUo+Trm7Ig/g8KI8xwm2ThO83GnXLyu5qoIFLgjAtvx9w
            uh/ZGIn2MKHy0aZ6J/HqDEbsr6XC/YpLb3mA3C5Msaiand0zmAh1oYQVvNJMLgLA
            HgBr7a14ngyndwGiBoFDoHu2gtPXTallruv/eopnOVaidkyNRDlMhbqr/Xkxlwov
            E2pewl+IKvt5WnGzCHDFvHYCDpeKX9ZAiBBJQ5tgGhxScN5rJ4Omx7iVbnjjPMzs
            1VSRgOqR1xPk5aMa0ByV2P978mNJL6MwIEhnGjg6Dyr1hvmjFxKjj+Pd8IWAeli9
            G3Xq4xJ8+vRbFBoqzBuxcUOTN/V1i1XECGMxEg5cE+9tp+2mvOSpiChkpxeGA42Y
            KbcVR7df2bjIQ+8IQzgPkpGnpG/XwC8JKsy+2jiiXOWrwUDfEFrkFaqGNareTeST
            ynkbl+y8PgtoHloubckKoXqyY/zHTG3gDDW7SLfr/OpHqyq8MtITyojwMB/Ijyzo
            6mAPiTLI7oFYpWIP0UiM7u4o6iDW9S8G9l+vLZJyEmhEUZJUkWoXRy2Ibd6ix0L3
            eA6izpRuehl1OLePY4HNkuqOgXiEf1mgNcoGnyx3kzKYa1cUlMP0ve8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "dqJq+qESCNakC3p9duc5LrG26D1scj58Hy1S5kPGtME";
        };
      };
    };
    kfbox = {
      owner = config.krebs.users.pinpox;
      nets = {
        retiolum = {
          ip4.addr = "10.243.100.102";
          aliases = [ "kfbox.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAis/ORNebUUz/yBTjouXVNoBHTVaEcpZE+cD9EWv975k0evxxb4s5
            W87fZ6YkQgYtgFScaBMgqDbUdi/zMAkhJb2cn+fOGvuH7QqfiuEdG+lncllN9JlS
            LpkP11COWmf60ThFieOa39xOT6atZc4cE0MMhw7CJUdIK7Y/EsHH1rVCWFdAKRIw
            4tM7H2Goi7+Tp2guRVFnM/lzRPnIbGOUQxrkMDB2gmpHdCpYWfAQZGzvxlIqrazC
            oPcaF0Pk6URkxGWeRUqJnwcGWxxaiA1KW+okma+Mv5k1DMr8pIK0ywWULMkNzjpP
            IIJybkeKomOaQmzjZ2RKulOS8A4OOxiDw9Y6I4x+1qFa1seAmYWWafnHCRPQb40g
            q5LCTfJaSvYfEEhs9xfo7vBd4IoYsymIQpLUV4BeNINazpRwNIb2AmHSknMnPZFR
            Hu8kvN/D6vNlSt7hR+sWU2XgNTJV/MizvG/6N+9wGSPL15dmt8XiSCjSv2mzBSaW
            7bVFoa57Y0OOwZUeK9SRJKMKPfxcNH9zGTMsb0Qnn0zFwscNnkDyA8Z4zPt4GpCX
            b3qlAwKFHHVByuDiWrjr6zfSKU/rEU/CRBoWnTcIwSZRhxvAiX82XgUf0qDtFKY9
            Wh+K8/4tfwX9niGTznIIic6Qmbm1O5MNjaPSPCK2bEgEMhQt9YkA4scCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "fXVRppte1zStbyfrbgJwamJXZnHYJkcC15f90u0sUJM";
        };
      };
    };
    keller = {
      owner = config.krebs.users.qubasa;
      nets = {
        retiolum = {
          ip4.addr = "10.243.30.2";
          aliases = [ "kelle.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA3jJgnaEJnKiBILtdtIROVfJJ1IgQSdfAw83aNE8xinkIFkP8lSFS
            Nd1C9pRI2r8Tjut/MB0b7MRlwOS2FWP1COcKzZGR4gKSiwK9oWGy6Vf5Qvrsd5M+
            0roUsf6Km/muJgqhWYY4OOaDK3LSp4mAo8H9+pibH9GuMuhu/Ebe0gtwnoOuuQs5
            GeHtaBrtpiGX2WvIU2S1TwDw0cmheEbqyaQ9COSqdOW1ldbfAbh7Zv38iUzMNXJ2
            yAWUfT5eYsIWlQc55JzEABuxIZEFj7BiR2vQYjVa+sIjsb+vI/6SFK4uiuqPP0dW
            xFAQyRuQbW0gyooMLXnZ6ByD/t4mFpk7Eo1Sxiv8CdgDI/lELZ1h7jTYKrcuPHYc
            P9m2Ut9FxuFMl+s2etkVUVGba2Kz9b9iwvvAZUtU85UrsQCkrghIT0Hm0SIdYQHO
            +WyCw46okk5xLicXEd+RgwlWWq+AJeo0LKof3uoRnjQq1kkU5E0nGX/YqRa3YIxV
            qmShTnQSTGUe6qVz1uAoh+ljTEUWWgW5UKuHPn1gdqFcIJ+4DSkJgiQ/cbSXtyp0
            35bQuqjpFe/bwW1PuK6YspMRK2hQrYkypQNrvjcz0RJJc/1ULILTl0NaZEMtCcj2
            t7KpA6wY6WIz5+uTVBnc3vQrcBebfSWzl0IWxjaSufp8ojq5B7mz8s0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "HeSMxgGaB9alyS0n766TJ3qA2fAwvJmMyLPFbYhfZdJ";
        };
      };
    };

    ahorn = {
      owner = config.krebs.users.pinpox;
      nets = {
        retiolum = {
          ip4.addr = "10.243.100.100";
          aliases = [ "ahorn.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAyfCuWUYEqp4vEt+a6DRvFpIrBu+GlkpNs/mE4OHzATQLNnWooOXQ
            4mncdpx7OKf5jKxQY6NytW2ogRTEr8F5B52O5jE4OAoj64WG2xhuzO82MDIuVJ0h
            ihiiVZ2O8Dx5sfhto7sr2Z9bsbpAZ3lSZC23I+NXk55KVxwl7YPzmZGD/dXLy/OC
            R7KTvNbkO5T+BkcRpeigSV/ROymenxbpOoEFZb9PXpE4NJCOaX1ZnUrD93xVUhh1
            7aHqqA3iWqjU8AK7Xp2Hm06pHNVjP0TfmleGtcCt47D6zQytmfjGwptdva4RqMfT
            0BWvjGoQYDmgLveYIYssWlcjfvn9oRRvlFS6QeUZ8pP/YsvgnR4wfILFbQMKvGFn
            OXrmZ6vG2rqmJCGfuo3sd3YdhPwHWDmNz0ORJRQ8EcDAblfyjkGS8CZvC/Cmh2vU
            bPEEl78g30Kpd8dFpym24C8LwtujK+rzk6EJJrfu0DAlxlDGJyGC89yKktkYV6Mh
            Cy9Mwfz8eFRF2IcwEJNgi10/GMiN9LYk3R49wQN/6poQd62cS0C8bBkeWIgvSn5Q
            zpvvg7ChjmvDc6rxiO1XXWODXVWFogu6IxMRKUgxk9EheX0UEu2ZpzalqmQqPm9Y
            J1rBAUDan+au0WkocTbCIB3Y18byvrRuegxeny6XzS8ECFnsZSyWzo8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "rMX99xOg69naxQoRc/wHCmaHC5aq+7vjwpzjK0z73KJ";
        };
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
    petrosilia = {
      owner = config.krebs.users.jan;
      nets = {
        retiolum = {
          ip4.addr = "10.243.143.11";
          aliases = [
            "petrosilia.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAxDumQ/06Yd3AQPSlHH9/kNngbc/tq5yBuT0ymbQGMHLL9X3pCz/f
            y9GZVpQtaKm7EZ0Kj8ieaPOyG7BItH0AvTdSJV7rn4WKuKfe5E5S4E8YqsZfSu4N
            IdEKVIisyBNCklXaDn6A7nxeUauwHQHuj0wOAnYKfaU+2haL+JzcFtQ1RpxDBsy1
            FbcEXO5NOhsXK4mHjtRrK1GamnCo5gvJU3w1NrfLRXteOOBsR49HhTIWvi8L4tSf
            fd/mFwWayB7D0feLhWBpMPQTa5TeeQPxhgJrlIwXJiONG8GWFWNCHEjbQaCuJJWn
            e37n9xCpdH867P921Ei+gyKZi9t6d+U4blrCpQzIe95t8Uv0i2c+YNt9NQL5Z119
            jt/Xhm7ccT9FeOuYsbjcO6g0BJumILEjD309vfQfWNims++vMd53q3dzxp4Kau+f
            vdMyrzWiIytM+/iQmneG8XLv0b7I6FUPEahpCncZ14NqBDaKclwoJ/HfB+WZi6JV
            yBVJHm9vogfzD1sLmDctHps3uJAeZHzszws8LMKdd5JxxQzVBRcrD1LKHYmmUYTU
            5gyDxnFn8ZoZ3GFVH+5v2PJgZY++/6zdDxQ9flrdt2zRaoAq2Zayn7R8sQ/ZjMXK
            eR8aXgHzEL/n/9BMKs+jLu3j8xaiJX8ctnRvwSnOFjU9wQvJ7QNQHk0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "Rs5jdJk/YF4aXohp3isau4LHinD4VWlvSa9CcgznR+A";
        };
      };
    };
    tpsw = {
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
          '';
          tinc.pubkey_ed25519 = "3IKIoZqg0jm9+pOOka2FEtihx0y8qAdJqKTuRfJtMpK";
        };
      };
    };
    verex = {
      owner = config.krebs.users.lc4r;
      nets = {
        retiolum = {
          ip4.addr = "10.243.232.232";
          aliases = [ "verex.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA7RCGaxVcTK3cPIs5NkbDdKEg/ASLRyKN2tBklvs43fD2lq/t77YK
            vtLkZhJokcxzDWNAyUZXgTsmVblYTzbyg+DFhygNwhMSI0vdrG5AoYhWa+eIe8mf
            Hxi7TWNTbDx/p66kw2NFDlw6Wbs5enPlMzfZPZj+aI7Dx7GrZRz8TrsKAauSSBKc
            Vtl7Aqs2FLk8suiMAOE4JD4Lt/pvR7YSISBo1N6/eBbFEosY1XqYkv+l9a0d948a
            k3jfJYRllsBRQzUyseMewwgVEz8Ny+rwk2J4ukSogAlMXXkPD/pYQgdTZwbGWOyY
            FMLgb7qULn60aUO6mE/mW9JP90/9cX3CD9McdEFRXV4oM0P9EUq49kN+vinD6JDm
            bL9fP+yx3sdzl34dFWDRPwrzn13kTDlRbble8jATRcisxMT1zYiADuRwIx8AeKs7
            O4uc7r/hz8ANO3zksuPhkTUoObTvZyW4mXbac2p6DGv/2aC6jzMRFJsJbWQK1TSr
            9WjeAOknhSP9UGxQWz6AgHNjq04dR3lQk34xMfKfWxRAaMD+T6frWKz++Z07WpUo
            OkPlz57jPZ7yeJGwwPM/CMcNNmA6YCqgE2kJo5rVQqlUb90nVRbuiQYYldl1YCIc
            Z4X36TKEXPBTiiKf6rFL6dJ64vaVxmOPr3+jdvLSufa/L7uXq3g66ZECAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "9ifWNFwaXe6qLVTW0UrOl2jg7erwTUC7f50Th4Vv2iE";
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
    pinpox = {
      mail = "main@pablo.tools";
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
    xq = {
      mail = "xq@shackspace.de";
      pubkey = ssh-for "xq";
    };
    miaoski = {
    };
    filly = {
    };
    pie_ = {};
    domsen = {
    };
    feliks = {
      mail = "feliks@flipdot.org";
    };
    lc4r = {
      mail = "lc4r@riseup.net";
    };
  };
}
