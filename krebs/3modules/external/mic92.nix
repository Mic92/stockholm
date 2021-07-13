with import <stockholm/lib>;
{ config, ... }: let
  hostDefaults = hostName: host: flip recursiveUpdate host ({
    ci = false;
    external = true;
    monitoring = false;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum.ip6.addr =
      (krebs.genipv6 "retiolum" "external" { inherit hostName; }).address;
  });
in {
  hosts = mapAttrs hostDefaults {
    amy = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          ip4.addr = "129.215.165.57";
          ip6.addr = "2001:630:3c1:164:b62e:99ff:fe3e:d369";
          aliases = [ "amy.i" ];
        };
        retiolum = {
          addrs = [
            config.krebs.hosts.amy.nets.retiolum.ip4.addr
            config.krebs.hosts.amy.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.181";
          aliases = [ "amy.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAr3jQRA1+hLKYVgHJA2ax5W8J3GVMTnaGpYw9Q2xXXrX/jxLZ6Ia8
            hBjIcCBDVL5Q3FnyrKB9NJeeIvCOKg8WG+8O0+wKcePKd0Vhbsx4Whog/6PWs6qh
            q2sURs2tp1hjHks4kZo2WtiYD7Ue9HHdV6FlUO6yuBV0bW2RzHdLPCDSGxnQVkBM
            tSwAvMCZwvVBiv4m6RyMXqmpdbAPBzgJcmJS0FY+zGxpiwsR/AdoVvnzYyFMCVpG
            iFl5+k9OGhUJq72MwAXzjW5ZdCPrG+2Dd+QBhhtIMJGA2sJiJteT8vdvpTNCiHJ/
            HnW7movliN2mW86qwo7QqB5v0c9f9TjfpOld7sS/4vE3zlGi/Stf6SQWaoXez/u3
            /P9GzupcYgj76m8Z3j7BMHXCBw8iwP2pZpL9hnLdIyCcyLrzXDIzq4hlt60DPhSU
            klTDBUA/cUdSJGcSn2N+WHLOTfI6qeBNKqcTk70OQsa69jAJeAtA+I9OprNYOXqb
            MmQakNNlrTaNtGQxfQqEL+wqHlo8CVDGm3O9pQSNF309P4TLNU1EYm+ItScNiVCE
            DKhcgvE6xHCwZnVyJN8MMy1CVyDmnHVYoaTEZ2cCvNi/hXIXgO9KWjSpAv5tP764
            UkOE4dlDpEW6G1pNf84BERfRYGDj29A/Jk9LJC/6D09QJXNu18HR0sUCAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = 6VktF9Fg9E0hCW5g+rwGnrPACPSx/8vkl+hPNaFYeND
          '';
        };
      };
    };
    clara = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          ip4.addr = "129.215.165.58";
          ip6.addr = "2001:630:3c1:164:b62e:99ff:fe3d:70f2";
          aliases = [ "clara.i" ];
        };
        retiolum = {
          addrs = [
            config.krebs.hosts.clara.nets.retiolum.ip4.addr
            config.krebs.hosts.clara.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.182";
          aliases = [ "clara.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA07G1n2sA804nnjWQzq0Fi9i6kxJUo+jVJjtkm5unw3hjflAAd/3d
            WN+01GdJCk/gr7DfU/Xr5KnR39Z3ADoT1tbUb+i5AJZ5/8VHUwWM8D8mQAam6LBf
            UEeLxhVH8rG6lHaKwVi9oe4gPhgptUOzX/YIlJOMYDlYRxc7Wbj7YQOAKlPuTAjY
            Z5bLswfkqTMO0cioJNwwMCNWSMJf3jbKi3eTQ36sf7TDMEneNGSBUpeSjGddoNT/
            rrVIDDT8tGmtACKr+3Y0H+EA2K5IxdQKKfnPRR31RBWiTkEXBbaJzYO/ZV5/xlbN
            wmblskwq9d9IwDY7qeMctci+ZUZ3epG8MUwYa4faOrgmmkQpa5B+6UOMzw/WDJEc
            jTfvSzfPo4anoj8C+MOQYzRvYmp60YEZKomv2BQdBvpGIpUul8WAR2aV0K+wz66e
            mUamljAXmLiPxgGKduX5VFVuXzYxeMiBBujQCLTjc+xTB2EdwihxNX1rkxz10BDc
            WrgPV+/VVyThKhOvVCifWARHtT2VGcZazfQOW/y3ZmEPOYuc5ZvrSEiMeG3f64+v
            UU8cQZ3yBLIhTtC+38pRlsdBQHt526q0j0rrnd30JXVAUdWBunP2UJ5QGtA8/mWn
            cWSlvRf5sfbyrISz6+mLPM2qGHnCkKwORNxmv/1DY07O3Rn6hX0OY4ECAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = qnJmS6W7QSKG3mjW1kPnHGeVmKzhGkyP9xBLGwH5XvD
          '';
        };
      };
    };
    dimitrios = {
      owner = config.krebs.users.mic92;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.183";
          aliases = [
            "dimitrios.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAutdjBACUieeP6hPqLazSo/MG5HiueUu3WZ1qPwpiPfJpPT59GckD
            SI+TfCzaaZrifh1sRP30QhOH9+ca5DPPNQuk3ZPVAS2dqSmea0RBnYgq1J9EJ2Ty
            EMzAYWjKIT8sJiEh4znnq7DDsd/JF5nIbhwgpkytxqAH8us5ABB940RkRMwDUS9M
            tWB1NCbS7q1JWEoCHguAbh4B5qv4gxwDqzj3UwTR1Fd+SO3o9/giKhvpk0iQfsDO
            DGXgxnpXybr7HGdRH2u3uAKXlwzwOpLHlohdLRC5txK8Osl0zVNqiiiV9SpuS0W1
            OrHcbfEuPbuuI4pOXKMoZxbaehQ4SmEVBwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    dimitriosxps = {
      owner = config.krebs.users.mic92;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.189";
          aliases = [
            "dimitriosxps.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAz9aKIhzk8+ZNBQmU054yc1yTdMyaw1aqWXYyQZoCmFaBIlMvF8I0
            dd+56cGjK8O7KkEhheDL/ijj9cCcxbqHSTktXz47ScyTaN63h13+MBUIUzDwSO4E
            9fRUUn3lbZenhGoON7hlaHb/qAR0yLxip0Tw77bcq4hvKleD74NnAJILPoP1KRDY
            O5vs8C8wpdJUtnlsfkAa058wDI+7GNPb0cs0/pBQVR2GUGb1xqVJ5obO/lFKOJ/e
            DKemnlg736cEaIF6v9M+w4VmL8mNudDy6RxA6/xIErP5Ru2aK5lH5UBHVCwdLLCy
            8y3It9Tgji3G9nOFbhaeKDjeIAJ8sG+WjQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    donna = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          ip4.addr = "129.215.165.54";
          ip6.addr = "2001:630:3c1:164:30a2:6e7b:c58b:cafd";
          aliases = [ "donna.i" ];
        };
        retiolum = {
          via = internet;
          addrs = [
            config.krebs.hosts.donna.nets.retiolum.ip4.addr
            config.krebs.hosts.donna.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.180";
          aliases = [ "donna.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAnv5zVPwjHk5Q72D3tv2rlQkp7SOsZD7Wvz8l1yI/mWkxoriJ9MVa
            x8RziSB3KF8sF1lRWIKmuynkgLI3w0X/YFs/fAvtayxk6Qf8DOl23Vd8Is0h/i3I
            0fCmCEIHhHboKsREW6NxY7w5WAI2+SFNmGef1P7vzrAv7iLyPbo9nQ8wlrAmc+PJ
            Ao3BOf4U7kP778fhsPA4dlGtF2v9CBhygeGVI/DQR8jcvzeiPd2Dr0k/JvrVMYtf
            wJW4xUwZkIpws/yfI8b4VJOFl2X/Yw9712Z8Jvga0rR32OG4YbnggvuCMum1g94k
            YwMjaSckv1XTalvPQuf1Od96XzwL2hjPFpEK3Tdl4AitMnArgj9HNzhcRL+eGonf
            U24zk52OToHnoP3palNpodi7DziIBeXIaIMl7VMXku2ymbOUJsI6zeew+uZahJkv
            QIWjxveQ8N40BoTc8Yg6pea1AId3l4f3brtwJbQOVbb3bVQ5VcrxM9Q/TBvyADYR
            Knwszxw3uBw5Za1FMbwCPwd8/y/Ar19qGCx25xK0QnsyqZZT/cHsbBOTzh6BBWwI
            IzbYu49VO/B1rktYzZ2l2ENQy6OILXWbvFjC8Pt8f1ZZQ4A21PyNA1AdyJ/rbVj7
            awm3OnnvKSvMCXWnwHPFHjksb3qMx96Aep1cw3ZBx0sQQ41UWBoOsi8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = ikUmx5IC1dvfaHFhpZM9xotwF2LH6EkvpcPTRm6TjeD
          '';
        };
      };
    };
    herbert = {
      owner = config.krebs.users.mic92;
      nets = rec {
        retiolum = {
          addrs = [
            config.krebs.hosts.herbert.nets.retiolum.ip4.addr
            config.krebs.hosts.herbert.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.177";
          aliases = [ "herbert.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA7ZINr8YxVwHtcOR+ySpc9UjnJWsFXlOyu3CnrJ8IrY+mPA25UmNZ
            stXd8QbJuxpad9HyPs294uW8UmXttEZzIwAlikVHasM5IQHVltudTTFvv7s3YFWd
            /lgpHbo8zOA2mafx+Sr02Fy/lHjk6BTf8IOzdJIpUHZL/P+FUl9baBwGLmtbEvPh
            fbvtf5QryBjJ9nRnb+wsPVpeFE/LncIMK/bYQsyE01T5QDu/muAaeYPbgm6FqaQH
            OJ4oEHsarWBvU1qzgz/IRz0BHHeTrbbP3AG/glTwL02Z1mtTXSjME7cfk7ZRM5Cj
            jXAqnqu2m1B08Kii+zYp4BPZDmPLT5gq+QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    eddie = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # eddie.thalheim.io
          ip4.addr = "129.215.197.11";
          aliases = [ "eddie.i" ];
        };
        retiolum = {
          via = internet;
          addrs = [
            config.krebs.hosts.eddie.nets.retiolum.ip4.addr
            config.krebs.hosts.eddie.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.170";
          aliases = [ "eddie.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAuRQphRlSIC/aqRTfvStPdJOJCx1ACeFIDEjRdgoxuu32qoBl7i6d
            j7Voh+Msditf2a5+f0fVsNDaPnjPGfk0NkZBjmn+RZQDRXk0krpTNj2Vb6W5quTm
            3yrjJMFJR9CU5khfppc47X+ir8bjn7RusWTFNEuDvUswHmRmnJHLS3Y+utOaRbCF
            2hxpyxCn423gpsaBfORPEK8X90nPbuNpFDugWPnC+R45TpNmIf4qyKvfhd9OKrua
            KNanGHG30xhBW/DclUwwWi8D44d94xFnIRVcG1O+Uto93WoUWZn90lI1qywSj5Aq
            iWstBK4tc7VwvAj0UzPlaRYYPfFjOEkPQzj8xC6l/leJcgxkup252uo6m1njMx3t
            6QWMgevjqosY22OZReZfIwb14aDWFKLTWs30J+zmWK4TjlRITdsOEKxlpODMbJAD
            kfSoPwuwkWIzFhNOrFiD/NtKaRYmV8bTBCT3a9cvvObshJx13BP+IUFzBS1N1n/u
            hJWYH5WFsQZn/8rHDwZGkS1zKPEaNoydjqCZNyJpJ5nhggyl6gpuD7wpXM/8tFay
            pAjRP40+qRQLUWXmswV0hsZTOX1tvZs4f68y3WJ+GwCWw9HvvwmzYes5ayJrPsbJ
            lyK301Jb42wGEsVWxu3Eo/PLtp8OdD+Wdh6o/ELcc0k/YCUGFMujUM8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    eve = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # eve.thalheim.io
          ip4.addr = "95.216.112.61";
          ip6.addr = "2a01:4f9:2b:1605::1";
          aliases = [ "eve.i" ];
        };
        retiolum = {
          via = internet;
          addrs = [
            config.krebs.hosts.eve.nets.retiolum.ip4.addr
            config.krebs.hosts.eve.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.174";
          aliases = [ "eve.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAw5cxGjnWCG8dcuhTddvGHzH0/VjxHA5V8qJXH2R5k8ki8dsM5FRH
            XRcH/aYg+IL03cyx4wU7oJKxiOTNGbysglnbTVthfYhqeQY+NRTzR1Thb2Fo+P82
            08Eovwlgb0uwCjaiH8ZoH3BKjXyMn/Ezrni7hc5zyyRb88XJLosTykO2USlrsoIk
            6OCA3A34HyJH0/G6GbNYCPrB/a/r1ji7OWDlg3Ft9c3ViVOkcNV1d9FV0RULX9EI
            +xRDbAs1fkK5wMkC2BpkJRHTpImPbYlwQvDrL2sp+JNAEVni84xGxWn9Wjd9WVv3
            dn+iPUD7HF9bFVDsj0rbVL78c63MEgr0pVyONDBK+XxogMTOqjgicmkLRxlhaSPW
            pnfZHJzJ727crBbwosORY+lTq6MNIMjEjNcJnzAEVS5uTJikLYL9Y5EfIztGp7LP
            c298AtKjEYOftiyMcohTGnHhio6zteuW/i2sv4rCBxHyH5sWulaHB7X1ej0eepJi
            YX6/Ff+y9vDLCuDxb6mvPGT1xpnNmt1jxAUJhiRNuAvbtvjtPwYfWjQXOf7xa2xI
            61Oahtwy/szBj9mWIAymMfnvFGpeiIcww3ZGzYNyKBCjp1TkkgFRV3Y6eoq1sJ13
            Pxol8FwH5+Q72bLtvg5Zva8D0Vx2U1jYSHEkRDDzaS5Z6Fus+zeZVMsCAwEAAQ==
            -----END RSA PUBLIC KEY-----

            Ed25519PublicKey = 7J1JgVyiy540akMdd/kONta0fMHSl5+FQJ1QhN84TzP
          '';
          tinc.subnets = [
            # docker network
            "42:0000:002b:1605:3::/80"
          ];
        };
      };
    };
    okelmann = {
      owner = config.krebs.users.mic92;
      nets.retiolum = {
        ip4.addr = "10.243.29.190";
          aliases = [
            "okelmann.r"
          ];
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAxquUuiW9a304H9Ls81+2BMm4bviDUU2Zogu0F1mPp6X8TpdjYpDs
          +tlakSTEPHo+aIdcV9rHpjOC3tirNbYU56D8DdoSo1Ra6XNFbxWrw7usSR9gz7L+
          kYp1Uij4gKTfg6YQkU0lkufk13if6zvb/GjoBUTS/Tx+8sZm2/JKEK8JLQaCkmMu
          LAUTsHj35Q8S99TzCLAoQLo136AtvPqcwwHVwkdX+S4WqtlODxfJ7T+9KFxGg54B
          1M6btg8iL5sdTFrLIBi7oK6GuLK9izvZ4O9O9H2bStW6LodqPtw2v5WA8li+YJx7
          LBgLO4aAAA6bF9WFcYyKBh6iCX0WxB7LowIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
      };
    };
    aendernix = {
      owner = config.krebs.users.mic92;
      nets.retiolum = {
        ip4.addr = "10.243.29.172";
        aliases = [
          "aendernix.r"
        ];
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIICCgKCAgEAt/dCDTvJU5jugP+5pk2CNM8X6cOnFonJv2eS253nsmKI97T9FSUa
          QDt417MoqAJNEeZw7o4ve1fmdZmtfKgmXYdDJi2HSJCJoKY6FUgVOKevtzGg4akl
          4mKTy2z59CxyIbA41MHyLq18W3NLabQ41NpWGBRt9jvHQpZfd+wI8t5IIzdvFrKo
          JSOFRbzEBL5//Hc3N/443cUg4IMyDBTemS7/jaZ2/Mn+PVZAdoIPLEZjFeWewmTF
          Jd8Bsc2thzAREYHYnawhq3PLJSebMJd91pCdkD0NB0i59VKORcQTFady3fzE9+w4
          RSTqAdBTUDuxzU/B8g1dp89/qW+fVPiFuB5Pf7D9t2DgxTDAeSXMiId/4Hwa0B1G
          QCnCedz0Qk2UdId16BTS8DSq8Pd9fawU6qCmPY6ahSiw5ZQ6odMvDISb480cKj41
          pslLjhIItTk3WEs8MwnQCzweNABuCK7GzT7CNaYm3f9pznBlOB+KfoZ6mrlzKkEK
          u+gFJXTFym0ZF0wheXO7FCJ1jp4LFHqKGS3zWQyT7isjLsbcQzpOe8/FdiFlQvlG
          vltL+5JjcahAMHc/ba+pRa5rSy8ebqf68fg4jlkT94Za13bCIHdK5w7eAXR3s/9z
          H2wZmhvajUIZAxQSgFUy+7kKWOIkWqFkGPIdmbdwTaHC88OWshvRv8ECAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
      };
    };
    dimitra = {
      owner = config.krebs.users.mic92;
      nets.retiolum = {
        ip4.addr = "10.243.29.192";
        aliases = [
          "dimitra.r"
        ];
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAtgvjWP2KIawJDk32P8Uiwz95REACx43CXUIgcBx5qg9ZQrHnJZxH
          RkXLnWUmjpnEmPUfvg/b8YCyoHgzD6GQEXcWaiMXBQ/nsrSEN4mpY7tzInerzGsv
          /M66WzPUWSUC9kbncLXt+2A64B23h1ki+MyMyKGIpHq21+F1b6ZHW2rkMnk3BKa4
          aJKNfadjP4V1lnPd40VBpcA3dlQfGF057GJz+2fzlfh1Bp41r/uP2NHieSAlyBws
          IaVZPWbfxFyYU8JbrlYUAlLjdXFG1meo5On0K0N8tTBKfnD1nwSqTPAfM7WqOm4A
          ImYB8LzjmIdXM+QUqbVFTgiY4jBDg61krwIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
      };
    };
    philipsaendig = {
      owner = config.krebs.users.mic92;
      nets.retiolum = {
        ip4.addr = "10.243.29.193";
        aliases = [
          "philipsaendig.r"
        ];
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAyWdCrXD0M9CIt0ZgVB6W5ozOvLDoxPmGzLBJUnAZV8f9oqfaIEIX
          5TIaxozN3QMEgS0ChaOHTNFiQZjiiwJL/wPx1eFvKfDkkn7ayrRS/pP+bKhcDpKl
          4tPejipee9T2ZhYg9tbk291CDBe1fHR5S2F8kPm8OuqwE2Fv9N8wldcsDLxHcTZl
          +wp4Oe/Wn5WLvZb3SUao17vKnNBLfMMCGC01yRfhZub41NkGYVWBjErsIVxQ+/rF
          Y7DdCekus+BQCKz+beEmtzG7d0Xwqwkif51HQ05CvwFNEtdUGodd8OrIO+gpIV6S
          oN+Q5zxsenLo6QRfsLD+nn7A7qbzd57kUwIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
      };
    };
    martha = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          ip4.addr = "129.215.165.53";
          ip6.addr = "2001:630:3c1:164:6d4:c4ff:fe04:4aba";
          aliases = [ "martha.i" ];
        };
        retiolum = {
          via = internet;
          addrs = [
            config.krebs.hosts.martha.nets.retiolum.ip4.addr
            config.krebs.hosts.martha.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.179";
          aliases = [ "martha.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA3lR3Wup2yd9SYs9n9a7lq/jXxlKdwjgp9gPEirLn3/XCFM7NpLIp
            LRm3Wdplv0NWim4zI3AsdGmUBrV3y0Ugj48Td4RpXlOiFjS8NHnvRbamCZF7m/pJ
            3T/QpQx98+QEKXb3gZ5aDGgcHLRbUYUBuwFOxAKaikuDe2qJxqXqOmA7RXZDkEqe
            FrQE/H1/+8HqJ1vhgZKi3Vu7zLRB1EV8nggWFjQKR8o0AeViLwM3OxFtGyKTaXuK
            WAQrvSdKQDpQwqAPogyeftGesOfW7z0xrelkux10p42YM9epYvZDFRG97/nupw/S
            iYGiTTFDBDTzpyT3zl1uwhmQ3re/nJXf5e4fgnZEcsweU8ysHtDhbimqrm9impVn
            XdKnnuNa9F8VlyHCT2pVC9+WDKDNtA2M8f+8lG8/hoJ7hhp5HhBZ3ncROyQqOg4F
            e6YtaFidi+fYXjQkdUXHv5FCkqFJnoxZdI2vwqU2DumltG/o+qsksI2WSsLsuMVs
            sa4KUq0+5OsmCJnIAKWV2YwbLVf1tJMjPGA0jQECrHPL6SKobRefqav6MPuTbytC
            4frtEIGbfdKqQ6nNTvTpCrAo+WAm3NE3khTYqGe4LqX/JMoGtWXp/Ex9IdG+sflM
            mESMjuHp9vPY4aZGPtYPP93Cxv3q7gm+EfIGebajISpaG28J+XjiNNsCAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = emKq1mfkW4/aCoCwmeFU3DtppKs+KsTvd9YGoFkFgdC
          '';
        };
      };
    };
    matchbox = {
      owner = config.krebs.users.mic92;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.176";
          aliases = [ "matchbox.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAqwB9pzV889vpMp/am+T0sfm5qO/wAWS/tv0auYK3Zyx3ChxrQX2m
            VrxO5a/bjR/g1fi/t2kJIV/6tsVSRHfzKuKHprE2KxeNOmwUuSjjiM4CboASMR+w
            nra6U0Ldf5vBxtEj5bj384QxwxxVLhSw8NbE43FCM07swSvAT8Y/ZmGUd738674u
            TNC6zM6zwLvN0dxCDLuD5bwUq7y73JNQTm2YXv1Hfw3T8XqJK/Xson2Atv2Y5ZbE
            TA0RaH3PoEkhkVeJG/EuUIJhvmunS5bBjFSiOiUZ8oEOSjo9nHUMD0u+x1BZIg/1
            yy5B5iB4YSGPAtjMJhwD/LRIoI8msWpdVCCnA+FlKCKAsgC7JbJgcOUtK9eDFdbO
            4FyzdUJbK+4PDguraPGzIX7p+K3SY8bbyo3SSp5rEb+CEWtFf26oJm7eBhDBT6K4
            Ofmzp0GjFbS8qkqEGCQcfi4cAsXMVCn4AJ6CKs89y19pLZ42fUtWg7WgUZA7GWV/
            bPE2RSBMUkGb0ovgoe7Z7NXsL3AST8EQEy+3lAEyUrPFLiwoeGJZmfTDTy1VBFI4
            nCShp7V+MSmz4DnLK1HLksLVLmGyZmouGsLjYUnEa414EI6NJF3bfEO2ZRGaswyR
            /vW066YCTe7wi+YrvrMDgkdbyfn/ecMTn2iXsTb4k9/fuO0+hsqL+isCAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = 1wPa2cmQ4FUFw9289d0KdG1DcDuMNIYMWzIUnVVHu2P
          '';
        };
      };
    };
    sauron = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          ip4.addr = "129.215.165.75";
          aliases = [ "sauron.i" ];
        };
        retiolum = {
          via = internet;
          addrs = [
            config.krebs.hosts.sauron.nets.retiolum.ip4.addr
            config.krebs.hosts.sauron.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.194";
          aliases = [ "sauron.r" ];
          tinc.pubkey = ''
             -----BEGIN RSA PUBLIC KEY-----
             MIIBCgKCAQEAxmCryT4ZEhPOvdZhWhYZsRS7sz1njSh2ozh6iwXRXhjRjZ9tYZVQ
             GoYc6ADnWCnb9SGpPe1WqwFMblfKofnXCvC4wLQaFsch1GIMPhujosJ4Te84BHi1
             XKqyompotE2F7iWYPE6i6UAdRK2dCapfCbiDBOjMhCnmmhM1oY5Bv/fBtx3/2N7E
             W+iN6LG2t9cKibs8qrLzFtJIfWn8uXU9dkdhX3d9guCdplGOn/NT/Aq3ayvA+/Mf
             74oJVJgBT5M1rTH2+u+MU+kC+x2UD+jjXEjS55owFWsEM1jI4rGra+dpsDuzdGdG
             67wl9JlpDBy4Tkf2Bl3CQWZHsWDsR6jCqwIDAQAB
             -----END RSA PUBLIC KEY-----
             Ed25519PublicKey = Z5+fArxMfP8oLqlHpXadkGc9ROOPHBqugAMD2czmNlJ
          '';
        };
      };
    };
    bill = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          ip4.addr = "131.159.102.1";
          ip6.addr = "2a09:80c0:102::1";
          aliases = [ "bill.i" ];
        };
        retiolum = {
          via = internet;
          addrs = [
            config.krebs.hosts.bill.nets.retiolum.ip4.addr
            config.krebs.hosts.bill.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.195";
          aliases = [ "bill.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAvzM5dWPpmzzmogjuZC5boNvz+MJcIO0WnE9IINBY+CLSw5ZpNDVB
            b97EG0Irs92OLJ5eesdPdF5LIyfFcFHOpPN+NdVEfLDWpFZVgOYh4BRy5+JdEk6O
            ybcxLFIdgBHxahd3W27FxXC1ALu/AInAA2b4rwYoNBi23idj8+wtL4MJldkr5QaQ
            sx8VQxIMy1xY4AbKcHdOt/nMrPoU6GnE9ObdcLys5cGUl/7Vc0NAMK6RrFQo+jfn
            2N0uWA1hZPAfZEEKP91xiOiRSx15WG3q9R/rqPmBh6l+rdPyWdRKcPVndCzVDrgw
            WWPcR9A9Yzr0ZrpEIHOfrDOqb2Ur1HlrXHZRpt55IYOKwC7ZimZzKkMj7zl1t2Rq
            nC07IJS7OI38amgLI0PSFI/Mx+mAPdYjd0fDcp8q7reOL63QT7cbrOw+cyOzNzGb
            I7U7QaHaA2unOa1EYj5Ocd6jI1IyHqQe9FkUqgTaDVU44U3WEo/KY6FZfhqSPPHs
            PsFzMj9nOWUGUr0cAn7DloIfNL49voO1C4HaiEvvhbSFIT/8suq3JznFxmP/q+Ph
            qYbXI/LXzU2Ln1Abiu9m1OfxTmEOlH9C54zyUvkAfhjcD2/aZWc76g06Oj2L6kZ6
            EC9Ku7Hk37rVOgZjtXUjuf3eUAvImknQ/JMRM3YDQgmu4iU0tJ1UnqkCAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = bN+knMGCqK+HkdOucynEXxeqGFOS2u8oWLRDV/gNIZI
          '';
        };
      };
    };
    nardole = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          ip4.addr = "131.159.102.2";
          ip6.addr = "2a09:80c0:102::2";
          aliases = [ "nardole.i" ];
        };
        retiolum = {
          via = internet;
          addrs = [
            config.krebs.hosts.nardole.nets.retiolum.ip4.addr
            config.krebs.hosts.nardole.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.173";
          aliases = [ "nardole.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAyYIN9FYtTmJTXUlBO4QYp9J7SZbglMEq0QCMpF9xQvCqJHl+C1vm
            NzAswlhbaK5J1spi6+zUXtYJEVQyP1xesDlVm9G+hntS7woEWtuLO7VUL9whWINb
            mO0OmYIEaWTMPIOKPTgc3tYsUhk7dw962/6I81JQczCHg1z2ItsRho/Kwi/Jo2Gj
            jnPJQoRek45+xIzlf9Jx38ntioTQIaLuSw7/lplT1cHNcefLje8FQmVEojY79Ijc
            6Ij4b9tPln8eQErw2sANS6kSUOVRnVkfeRW+3a4iRtd8SzXJ+aX5TCsq910Z1+/H
            ClK91GctU0V11s/m8LCp/Wz+o+4Z89JLxnil/ZS/6NHsaHysQPFPbx0Uh5nASF64
            RoWhzp2CSJTC9/UJKdPIpIokMIEGgKjy8Up3nY4yjoUnf6SZfzr4jmXfRmYmVaMp
            cCjbMbxBo+MjfXlGRxJAFGkS9zO9/21SEDiWqfOVThg5jbBR/q9ysRGcXndS0ea7
            NzsCbU1/0StxxmZLpBRz2MxGSHqlZbwInm9RjsXbCGa32tTiUz8VxjR3LTUMU8AP
            xpPLaIo7TIPdkDvCFL+DtXB9lE2PDpnSHbxyXKVKqxmCW1i/+msrBs/gnQ9VjzyA
            L1Ip2MBQd+CFUtaj+VdhjfulvpVcpr5e3nZe7cl38qucUp46tbVsJ3UCAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = BA8uWkeHofZb5s9bNy6PjefKNZwemETWAA+Q6okKn1M
          '';
        };
      };
    };
    rock = {
      owner = config.krebs.users.mic92;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.171";
          aliases = [
            "rock.r"
          ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA0uhNk3XXVxQcIVhD1Ime
            9PY3QBIcXvwDlOrd3oUwyWTvZpUeO7yzIXdouAe4s0ohPIVq7Cmruj4ZrOGUCKyB
            oJpOziYSbL/IiCpXyOzWMLEwu0AoeFfbxig+5oZfwQ9epM2j902CgsUipJBLIg48
            BC9oOD+/iYEwsFPqQ/S0kETyQK5Ad+qv0lbU6/Kmify8Qplvpv/8DRdjsdLki1fU
            a6MAEw12OtHe6IWtlitPjFMBykTP6kkSp/eg0G2KZFVuEulwHGf9QT/eT4fZTMCC
            2V5Vp4rIr/hawmj+h4NIxniBSQcPAAIGNwZVC4uYYV1nd4iaI/T04rDJwte5WKHf
            EVxtlYt9RU1I/XdNRSj9gYyneVcVlDVos8Z93oUv1hIGZYFtNmGVna6lggOBPf/t
            BZ1MT6FKA4QX9JI8bQoNs18s8ffzyb07psNbH6YhpCygnhf9C7NR/CeI8BtpzJza
            1Qk731Z6bk6xRFKMuY2tRKlNCqPHULj44oTHB3Ki2B/bMlkguqSChfFzKIRASYO1
            SASSgddexjkjKLslxcLWhIqYrZhuhYlFyoeoMI3qQsey/4X5PUmQDxxhTT80+qvE
            thBNPg46joyLTq9E9ddf7t/0C6oD2DXY88N9bkztuK5dtYHmjajUbePuaTJtrKhI
            7MnLboZCEiSyvkVTTx0Yjf0CAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
    };
    rose = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          ip4.addr = "129.215.165.52";
          ip6.addr = "2001:630:3c1:164:6d4:c4ff:fe04:4e4b";
          aliases = [ "rose.i" ];
        };
        retiolum = {
          via = internet;
          addrs = [
            config.krebs.hosts.rose.nets.retiolum.ip4.addr
            config.krebs.hosts.rose.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.178";
          aliases = [ "rose.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA0h88uEcgVFhggGh3xqHySt8T+oDdoSN8ve4ZPmMzrGCD4dnlWcUO
            6uMiwE7XG667wvjB0J2RbCJ8n8/r6eQgp6sRfPzSQL/Mc74J+py+sOVOjjjL5wJX
            btrYmASO3GKUSMhGmM0IiwHMIPrmUViaREDrweF3bUwK45d/ocqpBkc+nF27kksd
            DMYjHMWRIkKuQaj592zo/kY1pAJ/yAvDPess0x1CLL6uDNbjTr2S/L7JHdzZs9Xq
            1+SGdVtqD0sWgSBKA0PC/Mi+Divd4PC1SoSL7wZRWD0Y2DNgj3+xUc7hAWRCw2Gs
            5wofK+qiwnyYAmeNYcyQfDLosKZF9hOM8U3UbxptkPLsOK3cfZoGoLQCuOryVDBe
            6GfJkJ49WfuSSNWs3WPWL6/6zmVPeGR0TvoMt02VQ3cKTmeIkWyTIzSVoC7wYv5D
            Dl8Xt3aFr9UFI2GxenesViyuDLi8cy2fOsM3r+gowXQtgEKoXc9W2vyPwIIlcWUJ
            QrKVsyNlkKKL0YjsnGazaEvqdiE30/Iq7f7VBnXnWXRLnZhr85HbTdDQnpT4GcEv
            W3jpl1y5zShr5Hz90QoYcUTsxg9uk/+yqKpwUySZ6Gh4q0bo5k7nkM9i8mCMfNGZ
            0UU94QmwS9RoV4Mt4pSLYRcCs0mVeEjLuIfTFHkXc6LCjBWMn8ICfeMCAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = 0O1LrgXAFOuei1NfU0vow+qUfim3htBOyCJvPrQFwHE
          '';
        };
      };
    };
    turingmachine = {
      owner = config.krebs.users.mic92;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.168";
          aliases = [
            "turingmachine.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAxh+5HD1oAFTvMWEra2pYrA3HF8T4EnkP917lIUiuN7xUj7sawu0C
            t1/1IfIlH9dbxgFe5CD/gXvokxHdovPTGVH11L+thZgq6hg/xbYvZAl76yLxj7t9
            6+Ocac08TQZYMqWKShz5jqTVE/DLz4Cdy0Qk9sMJ1++OmH8jsWgK5BkogF99Gwf8
            ZiI0t3n3lCZsm3v592lveDcVIh6hjuCIvFVxc+7cOj0MKm1LxLWbCHZlUIE3he4g
            nZu4XiYaE4Y2LicMs8zKehnQkkXrP1amT56SqUfbSnWR+HZc2+KjwRDI5BPeTS06
            5WHwkQs0ScOn7vFZci3rElIc7vilu2eKGF1VLce9kXw9SU2RFciqavaEUXbwPnwT
            1WF35Ct+qIOP0rXoObm6mrsj7hJnlBPlVpb58/kTxLHMSHPzqQRbFZ35f6tZodJ1
            gRMKKEnMX8/VWm6TqLUIpFCCTZ5PH1fxaAnulHCxksK03UyfUOvExCTU4x8KS9fl
            DIoLlV9PFBlAW8mTuIgRKYtHacsc31/5Tehcx0If09NuMFT9Qfl2/Q3p6QJomRFL
            W5SCP9wx2ONhvZUkRbeihBiTN5/h3DepjOeNWd1DvE6K0Ag8SXMyBGtyKfer4ykW
            OR0iCiRQQ5QBmNuJrBLRUyfoPqFUXBATT1SrRj8vzXO1TjTmANEMFD0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = bXEnZa/jn2ntL0R4sMsRd7NIoHgzrzUnJ3ReJUQ8iFG
          '';
        };
      };
    };
    harsha = {
      owner = config.krebs.users.mic92;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.184";
          aliases = [
            "harsha.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA9VVG+kwSXDmjLuNCT6Mp9xTCj9IdzgjWxkExEH/Jd9kgVNXRa+39
            P8OQuHXi9fC/51363hh7ThggneIxOs2R4fZDyUcWfzv13aik34U0e+tYjhWXig+o
            MClkK4/uhLrsk370MQVevpjYW23S5d+pThOm84xIchvjR9nqzp6E3jzjhyeQwHJg
            dM48y7XT2+7hLvOkkEQ8xLcd35J228wVSilsSYhye1D2+ThRDbjjEkKXnIeOmU5h
            TPNvn+U0lVdwUDYlS+XUhNl3awRdfzTYlPvUhTWv9zwSxS5EQjvgMqC/3/fQod2K
            zyYdPwCwEyrksr9JvJF/t+oCw4hf3V4iOwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };

    redha = {
      owner = config.krebs.users.mic92;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.188";
          aliases = [
            "redha.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAx7STxTTPMxXugweHpUGOeLUrrTSCt7j5l+fjNtArIygOGKEiAC5O
            s0G4WHK2IcrNnv7pxS09S5mnXywi51aAL+G2fKzcU3YgLFuoUN4Kk5LohMvBynEE
            a3kZK2/D+LMeFfpK2RWBPjLnulN29ke11Iot42TC6+NIMWiZh/Y2T0mKirUJQGsH
            RV3zRlR7YfIOdR1AZ5S+qrmPF8hLb7O08TTXrHo8NQk5NAVUS89OYcn1pc9hnf/e
            FK5qRrQFMRFB8KGV+n3+cx3XCM2q0ZPTNf06N+Usx6vTKLASa/4GaTcbBx+9Dndm
            mFVWq9JjLa8e65tojzj8PhmgxqaNCf8aKwIDAQAB
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = oRGc9V9G9GFsY1bZIaJamoDEAZU2kphlpxXOMBxI2GN
          '';
        };
      };
    };

    grandalf = {
      owner = config.krebs.users.mic92;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.187";
          aliases = [
            "grandalf.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAn1wLOI8DluJAKvscyImoyG0gjxyVC1/Ky8A63YO7INy0SYBg3wU7
            XPSbix5VJZdADQ382LWg31ORYjnDg40c49gCGLfR6+awgd+Rb0sb4eAz07XENXJC
            qc70oQrrXLi8HIfeckCsJHe514LJOMA3pU+muaMShOiSygoTiTlEH6RRrkC8HROL
            2/V7Hm2Sg7YS+MY8bI/x61MIagfkQKH2eFyqGG54Y80bIhm5SohMkiANu78GdngI
            jb+EGlT/vq3+oGNFJ7Shy/VsR5GLDoZ5KCsT45DM87lOjGB7m+bOdizZQtWmJtC/
            /btEPWJPAD9lIY2iGtPrmeMWDNTW9c0iCwIDAQAB
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = dzjT09UeUGJCbUFrBo+FtbnXrsxFQnmqmJw7tjpJQJL
          '';
        };
      };
    };

    eva = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # eva.thalheim.io
          ip4.addr = "157.90.232.92";
          ip6.addr = "2a01:4f8:1c1c:9a9::1";
          aliases = [ "eva.i" ];
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.29.185";
          aliases = [
            "eva.r"
            "loki.r"
            "prometheus.r"
            "alertmanager.r"
          ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAyHptaExEcSUjEJ+RH33h
            uRK0Ttq8mJLDosWFYcoQkcL9S54aO9kF1gRJAKPBHoOt/IGeOxg2LNYWK6UjWfUy
            LB9c42EQ1wWZ2jSJ0LJgYzjR9cp3dlo9aHSa//O6p6eLpXRo9QLf8+aIWhNW5+BG
            sLIMR5b6Ngc2l8xQS+wvMmvTWJt3LyfQ6AKiKwCjeyrUFiuw0VWSn1I6n7H+CZBZ
            f/UvSxLucy1e0rvbHoTITOflIAfA84iCHsHsZjVqrx1iyOMdPtY2sBPmWhtVemDo
            duwzUpIuaJnWS7JOB4jsYWm672/KfzK7yAivqxD19OwqfZ3nNQ7sEDb3p4udw2Lf
            0dqHwZ5Hoj21vs3XiXX/SHcSf5QLzpj1MWBkV3r1D8I8v3P5qUbLunCofp3d9GxE
            N0gK06gqbLNonJvC/WD7lxeY32Rh1wYXbzbD/X6aWe/oD8WMIl312hH4cHQHOnVT
            t76NISlYTPxwX5mfFsBm8t0GjnnWY2jLwaefk7N/CwoDaKhkhmw1oeAZMuRcDRvE
            0ecpO4CZ6CcYERLxoYHgEAj3cMkSrQ8dT6XS4b9EO4hW4zCQ3RK9xDz71+uaihuB
            6uuTTsn7s0PYBJDNdccOf1Qt8fqPPgzqUKqeUciHojYDDPTC5KQh5m2PBv4I4iIR
            LnKOqNUX7UCqbdaE/tfFRG0CAwEAAQ==
            -----END PUBLIC KEY-----
            Ed25519PublicKey = 7rbs+10zzfwOPj5RoS1i/01QXuw7uIHGOHIgsjB2fHK
          '';
        };
      };
    };
    doctor = {
      owner = config.krebs.users.mic92;
      nets = rec {
        retiolum = {
          addrs = [
            config.krebs.hosts.doctor.nets.retiolum.ip4.addr
            config.krebs.hosts.doctor.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.186";
          aliases = [ "doctor.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAx0zdjPX9C0fBQR+8kdlsBTuMr4KxWhqw4ARqW02oSGKJxY+D57oO
            ORVfjBhrvIiZJfXaY0M+/n+M4Bvt4r5ol3N1NxkT7vc0bAbz9Kk/0M8dlspNoSO9
            WW+mITVfxg/DgzDegjj4TOrsWC1jBjo4PVrvA+PnxZC4VucnqZZ55JHWAk/mPtzs
            PUc3mkn3e9pwwrJMQRy7qg9fbatljHCb/fJoDk6DiQP4ZRE/pCf4OYCx7huHibsd
            EMp7y5QJySmKwJ/XsS6yiHeYXLFwWvfReja/IRFL4RiDSW+6ES4PTEXxoLVDpqgv
            KF44qim4UBabCMTPVtZcU3Rr+ufBALKJCwIDAQAB
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = PmZ8i6lB0Ij/d8qjA0y3QI2rMAlrTZn1ES/hUSNNWMP
          '';
        };
      };
    };
    bernie = {
      owner = config.krebs.users.mic92;
      nets = rec {
        retiolum = {
          addrs = [
            config.krebs.hosts.bernie.nets.retiolum.ip4.addr
            config.krebs.hosts.bernie.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.169";
          aliases = [ "bernie.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEApH5nG/Lwe/LiBkdL38vk1QcjOG/kf8dUqifouB56OQqe+MXASTSM
            vhipszZqXVGgWRVrSH3WSZt0YAXTQQGEjtnAr6fSSnUek21omRGFgr47LiGJp9R8
            OuhGPQs1sykIyl3HNSvDxj2EfWrXO73bKQPYdGIlfJWmsL69akWGlyYdEK1kloLC
            ld5+eYICjiTtqAQ8snZQNaPIucW4cGOa0sATUP4H1jbDWtFCKE2/mR/gGo/W/opC
            oOcJM7d5mb63blWVp9Zji/Gb64QltR50N3qvwc6W5ANHXIV97jYcNhSGqTsV0CEd
            n0cqUqymh2e8fJdmbB4DvwqhWITn6nwuFOWoVCSFMmbiidyTm3RAH9ztZARzsQRL
            Nj8OmeAr+plrzNH7AJpSkz30zukawCnbt+qWjqXLULH4kxJfOwzVh+KDfLzy7iLe
            OWWrblgJZA2GHKzCC5zntNujW6Nr2AliSY2Hch2XfkLTWeNtclKIEXMkRxif5oxm
            XpEJJ3lqdXz9/e37R/mkWVrdhpVfll2/v5c/PlnKMzky2mgkGpzegO0IiQcdJjrl
            fuXAsh5UbnE5kt6vKL5aducScatyd5FRkNumKG5ji26eZR4lZmXn380JLDInV4n7
            SODZL2fQFBnSD1wTWcq9Q/luPh4FitzJUZzHexvNxR/KBZycZJtdVw8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
            Ed25519PublicKey = pjCpkZToBUBbjUNVMWfYJePZ6g7m7Ccr9WedfKEFsXD
          '';
        };
      };
    };
  };
}
