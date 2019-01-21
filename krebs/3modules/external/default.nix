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
    dpdkm = {
      owner = config.krebs.users.Mic92;
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
    eddie = {
      owner = config.krebs.users.Mic92;
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
          tinc.subnets = [
            # edinburgh university
            "129.215.0.0/16"
          ];
        };
      };
    };
    eve = {
      owner = config.krebs.users.Mic92;
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
        };
      };
    };
    homeros = {
      owner = config.krebs.users.kmein;
      nets = {
        retiolum = {
          ip4.addr = "10.243.2.1";
          aliases = [
            "homeros.r"
          ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAoZq6BwB6rV6EfTf8PWOd
            ZhEWig5VcK1FcH0qi7KgojAhGSHhWmtFlvRSoGpQrSFRN0g5eTnrrguuTiIs6djc
            6Al9HMqwSD1IOkqFm8jM4aG5NqjYg3in6blOFarBEOglfnsYHiUPt6T4fERxRZ9v
            RguEWrishNMSv+D4vclKwctTB/6dQNsTAfnplcyDZ9un/ql9BG2cgU9yqeYLDdXd
            vRvrWX9eZKGJvTrQmAiKONlSvspr1d28FxcUrUnCsdRLvP3Cc4JZiUhSA7ixFxn3
            +LgGIZiMKTnl8syrsHk5nvLi5EUER7xkVX8iBlKA4JD4XTZVyBxPB1mJnOCUShQc
            QK6nVr6auvJbRn7DHHKxDflSBgYt4qaf92+5A4xEsZtgMpmIFH5t6ifGQsQwgYsm
            fOexviy9gMyZrHjQDUs4smQxxYq3AJLdfOg2jQXeAbgZpCVw5l8YHk3ECoAk7Fvh
            VMJVPwukErGuVn2LpCHeVyFBXNft4bem1g0gtaf2SuGFEnl7ABetQ0bRwClRSLd7
            k7PGDbdcCImsWhqyuLpkNcm95DfBrXa12GETm48Wv9jV52C5tfWFmOnJ0mOnvtxX
            gpizJjFzHz275TVnJHhmIr2DkiGpaIVUL4FRkTslejSJQoUTZfDAvKF2gRyk+n6N
            mJ/hywVtvLxNkNimyztoKKMCAwEAAQ==
            -----END PUBLIC KEY-----
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
    kruck = {
      owner = config.krebs.users.palo;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.201";
          aliases = [
            "kruck.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAxcui2sirT5YY9HrSauj9nSF3AxUnfd2CCEGyzmzbi5+qw8T9jdNh
            QcIG3s+eC3uEy6leL/eeR4NjVtQRt8CDmhGul95Vs3I1jx9gdvYR+HOatPgK0YQA
            EFwk0jv8Z8tOc87X1qwA00Gb+25+kAzsf+8+4HQuh/szSGje3RBmBFkUyNHh8R0U
            uzs8NSTRdN+edvYtzjnYcE1sq59HFBPkVcJNp5I3qYTp6m9SxGHMvsq6vRpNnjq/
            /RZVBhnPDBlgxia/aVfVQKeEOHZV3svLvsJzGDrUWsJCEvF0YwW4bvohY19myTNR
            9lXo/VFx86qAkY09il2OloE7iu5cA2RV+FWwLeajE9vIDA06AD7nECVgthNoZd1s
            qsDfuu3WqlpyBmr6XhRkYOFFE4xVLrZ0vItGYlgR2UPp9TjHrzfsedoyJoJAbhMH
            gDlFgiHlAy1fhG1sCX5883XmSjWn0eJwmZ2O9sZNBP5dxfGUXg/x8NWfQj7E1lqj
            jQ59UC6yiz7bFtObKvpdn1D4tPbqBvndZzn19U/3wKo+cCBRjtLmUD7HQHC65dCs
            fAiCFvUTVMM3SNDvYChm0U/KGjZZFwQ+cCLj1JNVPet2C+CJ0qI2muXOnCuv/0o5
            TBZrrHMpj6Th8AiOgeMVuxzjX1FsmAThWj9Qp/jQu6O0qvnkUNaU7I8CAwEAAQ==
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
              -----BEGIN PUBLIC KEY-----
              MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA6ioASTOx6Vndp316u89Z
              f+9WgfyVGw9deP2pQjoHnsPjBqRrsDCQGFO/U1ILQn0AWskQpHWHRir7Q6cI90jm
              8MqqGVymVFbeYbrOLHLjp+2fle9iU9DfST4O76TQwF/3elLf3tpGFS8EB+qF3Ig7
              aVOf5TuHPWWj6VtGTuWW9I8MsPnNykyRstlWXEztIs2zQrc0cO1IGd1QVarDGqTs
              KR4Zm7PvF7U193NzPLaH6jcdjF37FETLrNxAu88M+YnvXBp4oRHeJmvBloazpH0v
              aSb3+vNRlViMSlf9ImpAHlFRyvYYDAWlIY0nyeNUJna1ImGloSStLtBAhFAwc65j
              kmrXeK3TVAoGZQOvSbjFmI/nBgfHEOnz/9aRVHGUNoQ/nAM6UhALFEZV6sdjX6W4
              3p670DEO5fiI3fqqErkscbv8zSEjfmxV4YGMXVMw8Ub87fGwQEF17uDLeqD0k9AB
              7umwrWP53YffauAqinma0I6RcLRVRfJ2vhyBH1mKwAAW55WU6DpBTydy46kxy/Oz
              k9Cnxw7oMydUAAdnf5Axgs+dcx43lnXvGsoHi4lZycYhqtPe2YI152HAbGfmrixV
              Slzh8aiinBkLYW2VzJNTRmHvB3njjeua4/guXwe00G7MIs3UDMIieJNcVxb+E07v
              vF2rqhqU9b+1MQRhIPsBf4cCAwEAAQ==
              -----END PUBLIC KEY-----
          '';
        };
      };
    };
    rock = {
      owner = config.krebs.users.Mic92;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.171";
          aliases = [ "rock.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAsMJbXDhkaLZcEzCIe8G+rHyLulWIqrUAmDT4Vbtv4r0QhPBsqwjM
            DuvRtX5SNHdjfZWnUZoOlmXrmIo07exPFQvyrnppm6DNx+IZ5mNMNVIFUoojRhF7
            HS2jubcjTEib56XEYWKly0olrVMbsJk5THJqRQyOQuTPCFToxXVRcT5t/UK6Dzgh
            mp+suJ7IcmmO80IwfZrQrQslkQ6TdOy1Vs908GacSQJyRxdRxLraU/98iMhFbAQf
            Ap+qVSUU88iCi+tcoSYzKhqU2N0AhRGcsE073B3Px8CAgPK/juwTrFElKEc17X9M
            Rh41DvUjrtG4ERPmbwKPtsLagmnZUlU8A5YC8wtV08RI5QBsbbOsKInareV1aLeD
            91ZVCBPFTz8IM6Mc6H435eMCMC2ynFCDyRGdcue3tBQoaTGe1dbduIZkPGn+7cg4
            fef1db6SQD4HCwDLv8CTFLACR/jmAapwZEgvJ3u3bpgMGzt+QNvL1cxUr3TBUWRv
            3f0R+Dj8DCUWTJUE7K5LO7bL4p9Ht0yIsVH+/DucyoMQqRwCwWSr7+H2MAsWviav
            ZRRfH0RqZPEzCxyLDBtkVrx+GRAUZxy1xlqmN16O/sRHiqq3bv8Jk3dwuRZlFu6q
            cOFu4g9XsamHkmCuVkvTGjnC2h21MjUUr3PGHzOMtiM/18LcfX730f8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    scardanelli = {
      owner = config.krebs.users.kmein;
      nets = {
        retiolum = {
          ip4.addr = "10.243.2.2";
          aliases = [
            "scardanelli.r"
          ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAxM93+YgGhk5PtcOrE7E/
            MAOMF/c9c4Ps6m8xd4VZat3ru07yH8Yfox1yM6jwZBwIwK2AC9DK0/k3WIvZQUge
            UKSTiXpE4z/0ceaesugLQ9KTjUty1e/2vQ78bOqmd7EG3aPV2QsjlgpjJ6qQxeFi
            kjlHoFi9NNBLVkIyaAdlAhwvZuYFmAY/FQEmm6+XOb+Nmo+fccQlG6+NinA2GOg0
            gdY/dKYxa04Ns/yu7TK3sBQIt6cg/YUk9VpyC4yIIRPMdyVcAPz3Kd2mp23fhSvx
            we80prWXYtdct4vXaBZm9FUY5y4SL3c0TEScuM73VXtr2tPAxjD5W4XMWhrjnIiY
            QzoyAquVS9rR4fCaoP+hw3Tjy7Att3voa/YlHEDaendxjZ3nuO0m0vcgOa+SfCNm
            SqLsqb8to1y8yJ8LnR2og4MbtasxqSe1L9VLTsb4k/AGfmAdlqyG4Q1h5pCBh0GL
            2F6FbYHzwrwqBvVCz4DTPygPtta5o7THpP50PgojtzNLm1yKWpfdcWeMgGQJSI0f
            m3yenytM1u0jjw7KbBG79Z3etFNIYZy4Uq/dryEJnwpTFls+zZn9Q3tDEnO4a38Q
            FgzV0VLQpRM/uf1powSDzoWp+/JYgB9464OKcTsSlVJpi3crxF86xFqqc39U2/u5
            lM61fOMcVW1KREdWypiDtu8CAwEAAQ==
            -----END PUBLIC KEY-----
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
    turingmachine = {
      owner = config.krebs.users.Mic92;
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
        };
      };
    };
    inspector = {
      owner = config.krebs.users.Mic92;
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
    matchbox = {
      owner = config.krebs.users.Mic92;
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
  };
  users = {
    ciko = {
      mail = "wieczorek.stefan@googlemail.com";
    };
    exco = {
      mail = "dickbutt@excogitation.de";
      pubkey = ssh-for "exco";
    };
    kmein = {
      mail = "kieran.meinhardt@gmail.com";
      pubkey = ssh-for "kmein";
    };
    Mic92 = {
      mail = "joerg@higgsboson.tk";
      pubkey = ssh-for "Mic92";
    };
    palo = {
    };
    qubasa = {
      mail = "luis.nixos@gmail.com";
    };
    raute = {
      mail = "macxylo@gmail.com";
      pubkey = ssh-for "raute";
    };
    sokratess = {
    };
    ulrich = {
      mail = "shackspace.de@myvdr.de";
      pubkey = ssh-for "ulrich";
    };
    miaoski = {
    };
  };
}

