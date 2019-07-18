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
    catullus = {
      owner = config.krebs.users.kmein;
      nets = {
        retiolum = {
          ip4.addr = "10.243.2.3";
          aliases = [ "catullus.r" ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA2tRtskPP6391+ZX9xzsx
            CUotXuqYucYmnUbrRSIlxASVqTmAf3nDOE5EDBBcTdSwnb02JcJW4Zh7+BGgMxjF
            GxDPs6ETI28mHK+6rp8TOkMnyDb5mtSGVZPvKJU9fFOt6aAX1J1BzTfwtHtVQq7K
            WBzdpeKXlw4dIQ6K6SGmPIPpEh9pE1Xb+GuVljCXKxGJFbW40dmh2ZdadO7umBDu
            vRk08jT9/BUnUP6KrZlvyePnG38z6srMrVU+XAHu5D2qZ9y+QIp3kw7Y5JUrNXc7
            9q9P9TYx15GiIz2mSJKcLVmkLRebsaqdV7dBibPbfdGE+NB+F1FYPGDdW4cnonon
            DzzjGm/FDfOCXEnSkYGQDBWpfd/8AWum1xGJxJCPNBJElGE2o5jDWo4Y1b9gHP0M
            vARm8AOK8R1pQ7BP+pNMO0gGw2NDrtWiWpTeZ7SqXmZAZ/Gmyen9X+/fowcbTyDH
            b9joIuMQeOtxbUV2JprZIdit9NBFSZq/7Re/GBUwjGBm3LabIXFNGKZovx/f9lf8
            r5tVs4SPauiKzZS0K1Gz1NSq+3OXaY5EwVrBUXptYqRT7uyhVloOPRUsqRFeB0Fn
            Y5xOpDJ0UiJxgFbdH5Vb81D/VjNO9Q4nZib8wSEuLrYLHGoceQPX4+Ov9IdhIL4B
            BMTCaF+VCWC5PCLr0e61KqMCAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
    };
    wilde = {
      owner = config.krebs.users.kmein;
      nets = {
        retiolum = {
          ip4.addr = "10.243.2.4";
          aliases = [ "wilde.r" ];
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
    idontcare = {
      owner = config.krebs.users.Mic92;
      nets = rec {
        retiolum = {
          addrs = [
            config.krebs.hosts.idontcare.nets.retiolum.ip4.addr
            config.krebs.hosts.idontcare.nets.retiolum.ip6.addr
          ];
          ip4.addr = "10.243.29.177";
          aliases = [ "idontcare.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAxmmbQLVXcnCU9Vg9TCoJxfq/RyNfzaTj8XJsn4Kpo3CvQOwFzL6O
            qZnbG55WjPjPumuFgtUdHA/G8mgtrTVaIRbVE9ck2l2wWFzMWxORzuvDbMh5xP8A
            OW2Z2qjlH6O9GTBCzpYyHuyBWCjtiN4x9zEqxkIsBARKOylAoy3zQIiiQF0d72An
            lqKFi9vYUU90zo9rP8BTzx2ZsEWb28xhHUlwf1+vgaOHI1jI99gnr12dVYl/i/Hb
            O28gDUogfpP/5pWFAHJ+53ZscHo8/Y7imjiKgGXmOHywoXOsKQ67M6ROEU/0xPnw
            jKmq2p7zTJk2mDhphjePi5idd5yKNX5Q3wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    rose = {
      owner = config.krebs.users.Mic92;
      nets = rec {
        internet = {
          ip4.addr = "129.215.165.52";
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
    martha = {
      owner = config.krebs.users.Mic92;
      nets = rec {
        internet = {
          ip4.addr = "129.215.165.53";
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
    donna = {
      owner = config.krebs.users.Mic92;
      nets = rec {
        internet = {
          ip4.addr = "129.215.165.54";
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
    rilke = {
      owner = config.krebs.users.kmein;
      nets.wiregrill = {
        aliases = [ "rilke.w" ];
        wireguard.pubkey = "09yVPHL/ucvqc6V5n7vFQ2Oi1LBMdwQZDL+7jBwy+iQ=";
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
  };
  users = {
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
    kmein = {
      mail = "kieran.meinhardt@gmail.com";
      pubkey = ssh-for "kmein";
    };
    Mic92 = {
      mail = "joerg@thalheim.io";
      pubkey = ssh-for "Mic92";
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
    "0x4a6f" = {
      mail = "0x4a6f@shackspace.de";
      pubkey = ssh-for "0x4a6f";
    };
    miaoski = {
    };
    filly = {
    };
    pie_ = {};
    domsen = {
    };
  };
}
