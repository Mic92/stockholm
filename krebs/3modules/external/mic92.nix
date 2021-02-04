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
          '';
        };
      };
    };
    dpdkm = {
      owner = config.krebs.users.mic92;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.29.173";
          aliases = [ "dpdkm.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAuW31xGBdPMSS45KmsCX81yuTcDZv1z7wSpsGQiAw7RsApG0fbBDj
            NvzWZaZpTTUueG7gtt7U9Gk8DhWYR1hNt8bLXxE5QlY+gxVjU8+caRvlv10Y9XYp
            qZEr1n1O5R7jS1srvutPt74uiA8I3hBoeP5TXndu8tVcehjRWXPqJj4VCy9pT2gP
            X880Z30cXm0jUIu9XKhzQU2UNaxbqRzhJTvFUG04M+0a9olsUoN7PnDV6MC5Dxzn
            f0ZZZDgHkcx6vsSkN/C8Tik/UCXr3tS/VX6/3+PREz6Z3bPd2QfaWdowrlFQPeYa
            bELPvuqYiq7zR/jw3vVsWX2e91goAfKH5LYKNmzJCj5yYq+knB7Wil3HgBn86zvL
            Joj56VsuB8fQrrUxjrDetNgtdwci+yFeXkJouQRLM0r0W24liyCuBX4B6nqbj71T
            B6rAMzhBbl1yixgf31EgiCYFSusk+jiT+hye5lAhes4gBW9GAWxGNU9zE4QeAc1w
            tkPH/CxRIAeuPYNwmjvYI2eQH9UQkgSBa3/Kz7/KT9scbykbs8nhDHCXwT6oAp+n
            dR5aHkuBrTQOCU3Xx5ZwU5A0T83oLExIeH8jR1h2mW1JoJDdO85dAOrIBHWnjLls
            mqrJusBh2gbgvNqIrDaQ9J+o1vefw1QeSvcF71JjF1CEBUmTbUAp8KMCAwEAAQ==
            -----END RSA PUBLIC KEY-----
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
    inspector = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          ip4.addr = "141.76.44.154";
          aliases = [ "inspector.i" ];
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.29.172";
          aliases = [ "inspector.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAr3l/u7qcxmFa2hUICU3oPDhB2ij2R3lKHyjSsVFVLNfl6TpOdppG
            EDXOapeXL0s+PfBRHdRI3v/dibj4PG9eyKmFxsUJ2gRz4ghb1UE23aQ3pkr3x8sZ
            7GR+nJYATYf+jolFF9O1x+f0Uo5xaYWkGOMH8wVVzm6+kcsZOYuTEbJAsbTRZywF
            m1MdRfk54hLiDsj2rjGRZIR+ZfUKVs2MTWOLCpBAHLJK+r3HfUiR2nAgeNkJCFLw
            WIir1ftDIViT3Ly6b7enaOkVZ695FNYdPWFZCE4AJI0s9wsbMClzUqCl+0mUkumd
            eRXgWXkmvBsxR4GECnxUhxs6U8Wh3kbQavvemt4vcIKNhkw32+toYc1AFK/n4G03
            OUJBbRqgJYx9wIvo8PEu4DTTdsPlQZnMwiaKsn+Gi4Ap6JAnG/iLN8sChoQf7Dau
            ARZA3sf9CkKx5sZ+9dVrLbzGynKE18Z/ysvf1BLd/rVVOps1B/YRBxDwPj8MZJ0x
            B7b0j+hRVV5palp3RRdcExuWaBrMQQGsXwLUZOFHJJaZUHF9XRdy+5XVJdNOArkG
            q1+yGhosL1DLTQE/VwCxmBHyYTr3L7yZ2lSaeWdIeYvcRvouDROUjREVFrQjdqwj
            7vIP1cvDxSSqA07h/xEC4YZKACBYc/PI2mqYK5dvAUG3mGrEsjHktPUCAwEAAQ==
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
          '';
          tinc.subnets = [
            # ohorn lan
            "fd42:4492:6a6d:500:8526:2adf:7451:8bbb"
            # docker network
            "42:0000:002b:1605:3::/80"
          ];
        };
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
          '';
          # ohorn lan
          tinc.subnets = [ "fd42:4492:6a6d:500::/64" ];
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
            "loki.r"
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
          '';
          # ohorn lan
          tinc.subnets = [ "fd42:4492:6a6d:500:f610:15d1:27a3:674b" ];
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
            MIIBCgKCAQEAqIc+ozq3hKHMe/X3v4j+6or8LMjEV7MtQ8/+n00xpG4NkI4G38Bv
            3nmAcV7OhN6of0fr0psbBmym+2VxCZbpl8E3g1GWSKpAvlmP/9v4wDVdrADaTvXC
            pzCxejtCwEhKLisnMwCMJCuUPbIsSBU+IQDPKP7NP0yY5VapgW3Xl3qXpnehCW1r
            NBZjZASnhSXcJRLJayEDN6uBviYrnnfbrHOx4fPcjQPTHX5RYr3EbgGZQO9xki44
            9dKT4EA95lupTqC3wzuQbaNpvIuVzmggiDY/NsBIVh0/2XjGnO54wtCEPudaLnWd
            WNtc1wfVFB6gzgG1N7msOuFUReOIfyF/ywIDAQAB
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
          '';
        };
      };
    };

    eva = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # eva.thalheim.io
          ip4.addr = "52.59.172.193";
          ip6.addr = "2a05:d014:301:a601:ef0e:5434:d814:b8ed";
          aliases = [ "eva.i" ];
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.29.185";
          aliases = [
            "eva.r"
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
          '';
        };
      };
    };
  };
}
