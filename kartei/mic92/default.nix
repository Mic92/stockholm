{ config, lib, ... }: let
  inherit (lib) flip mapAttrs optionalAttrs recursiveUpdate;
  slib = import ../../lib/pure.nix { inherit lib; };
  hostDefaults = hostName: host: flip recursiveUpdate host ({
    ci = false;
    external = true;
    monitoring = false;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum.ip6.addr =
      (slib.krebs.genipv6 "retiolum" "external" { inherit hostName; }).address;
  });
in {
  hosts = mapAttrs hostDefaults {
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
    rauter = {
      owner = config.krebs.users.mic92;
      nets = {
        internet.addrs = [ "rauter.thalheim.io" ];
        retiolum = {
          aliases = [ "rauter.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEArpSEoqFUdjaLiR3MpBlEoR0AOyaHPY9IPG4C5KsrfjeMDdfpOEGu
            G0VHksBbkDV/MIgUVlK1B7LxZ73WUwKKB1YWGtY+QVX1tzoUqYwjMhp/xFVybyBw
            M7nmTnM6Uq9Xd+S5mNMmOdvgNXfiP+zy4+iHJpn8YN/RnuyETqXhvVW9UasqVlmz
            cY0dl+wsYFsJDnGc2ebpx5dzfpPgZKIFc0GlqDX0AqdQ2t2O9x4G5sFyUH0qPnDQ
            776it6NXhwSKfl1h9xjQp8+qowIUlUqKgiVXfAzXHSxWmVQyxilCAkEk4vSs1HOj
            ZNiK3LJKWEsy61hMt6K6AqpvSGlOdGa8WQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "bL0slCR9oHx7FBeVb4ubo/bX8QJJBgchVKVSlWh3y1D";
        };
      };
    };
    eve = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # eve.thalheim.io
          ip4.addr = "135.181.61.171";
          ip6.addr = "2a01:4f9:4b:4084::1";
          aliases = [ "eve.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [
            "eve.r"
            "flood.r"
            "warez.r"
            "ca.r"
          ];
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
          tinc.pubkey_ed25519 = "7J1JgVyiy540akMdd/kONta0fMHSl5+FQJ1QhN84TzP";
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
          MIICCgKCAgEAvanhJvtvqnTGblOF9Dy7Un3vaLAJHGeu9z8YMARFh6ENe+duILp0
          IDjJMZc7F3J01RbkjkfbzPiXmHN532MBcbKnp0Z5eUld/XmDdNCc3ekTifrYs2em
          eJKFrx2Vhsx924PZ8cOOf7P+JuqJNQzMiy7ohATjpMLU9If1tjqSyV+/lGjbjckN
          /e88XtG7Z4Cu5LdbD5Ajb4Rzp9gL0ae4aNw+2nX3wMJLYEjOcmBYuMzBcLYzVnZw
          YrtgN9RV8md9gdb2B/Fj1PdJGDyjdiuGRE9LnloC3dpMSkmhbNm9DthsThaWMUn1
          DyrtHrJoyNTO8OvyTfWK7EqKqZcZ+0gaTmtec5VCYWSCpb/CWLmHL3ydTyzNhtRA
          9ZFRwPQUdBsYQ/G/xtGrMQf5T/FdqUj3bD5pGlw6vheabBkD8a8Bt7WB52fzWWb0
          MZZlxyWiHoIim83LI8Qa5WHkJ7jZkV8XdrwsA7hkJpVikJIbWsdzwQVWBVvz5WiF
          0z1vi/cb5EYe3MRRshhG5VpTHBJzDRmvkdbKqrWi8dFEzJGkr0NPflmVKYAIBnRI
          xLemDSacswrvY1x9cdzCsNI92SkYxCvsVI27DCeeF5cfkApkZ0YcnOJm+3joTgpP
          uF8mQiPsyavyuBg4QWWPwGJosDRbycmHEzGDRLoizSkAQX5c+rvCvVECAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "5ZhQyLQ2RLTkKvFCN38dfmqfjZOnZmm19Vr1eiOVlID";
      };
    };
    aenderpad = {
      owner = config.krebs.users.mic92;
      nets.retiolum = {
        ip4.addr = "10.243.29.201";
        aliases = [
          "aendernix.r"
        ];
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAvHSVUd6/5P2rK3s9iQhVrxkjufDIi0Kn04iVB4Z0TpUvnmFAP+Hv
          d7umo95lNkAPL9c3byv4ooQjOskrp7GmgQRijLUvJSAZ9FBVWPAjMXs+gk9oJnQj
          6bovXJ3DurmW3h1ZRmkWn256j7g8lEMtf5LGFxs9Bwi4wqZTbI6DzTQhmNm76Spb
          2UMSzr9kDcNj5r6LDhDKEDtx4P1Opshgsf9AusV81N5nqDcvAYsvEqYoPvjKIPwF
          5jtfHY7hM7SdYoVgdAY8RFH7xuRkLQW4LBxPKjP3pEQPCgXcuEELm33PGr+w/vhC
          jxeyKP+uSeuBBMSatTWG3kU8W2LxVML65QIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "jC2UzKiUtWUlZF2ET88qM+Ot+GpoWxFFfpi8TCCr0uM";
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
    yasmin = {
      owner = config.krebs.users.mic92;
      nets.internet = {
        ip4.addr = "131.159.102.7";
        ip6.addr = "2a09:80c0:102::7";
        aliases = [ "yasmin.i" ];
      };
      nets.retiolum = {
        aliases = [
          "yasmin.r"
        ];
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAnQ6HGgUPVQbDIsLZAawZu4vK9yHF02aDrIWU9SdzpAddhM8yqWeC
          f55W6zyjZuoQ2w4UNthDl6gjQM6A9B+nEMRNz3Rnhp57Lyi0a6HZHF2Eok9vJBiu
          IRbVUxPpPKOGE09w0m5cLOfDfaZVdAT+80lQYoaasDr2VlRJNa2/arzaq847/SVg
          vaf4gOmE+iIK+4ZDHqLcTn1WD6jy+aMChZU/zI31vZ8vM4oPuGh1xbcB3wKP3Vf3
          OTqpGN86CdrdBahJkzNJzIXYsPsRaZ2+8dWTH9gJjI0z+yywQQCrrh9K/oJtDUHF
          BwmNc150BoSLqwduSWLtBonCa9p2/y/TDQIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "ZQt/OcrDlQZvtJyMEFcS6FKjtumBA9gBWr7VqGdbJBP";
      };
    };
    matchbox = {
      owner = config.krebs.users.mic92;
      nets = {
        retiolum = {
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
          tinc.pubkey_ed25519 = "1wPa2cmQ4FUFw9289d0KdG1DcDuMNIYMWzIUnVVHu2P";
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
          '';
          tinc.pubkey_ed25519 = "BA8uWkeHofZb5s9bNy6PjefKNZwemETWAA+Q6okKn1M";
        };
      };
    };
    turingmachine = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet.addrs = [ "turingmachine.thalheim.io" ];
        retiolum = {
          via = internet;
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
          tinc.pubkey_ed25519 = "bXEnZa/jn2ntL0R4sMsRd7NIoHgzrzUnJ3ReJUQ8iFG";
        };
      };
    };

    doctor = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # monitoring.dse.in.tum.de
          ip4.addr = "131.159.102.4";
          ip6.addr = "2a09:80c0:102::4";
          aliases = [ "doctor.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [
            "doctor.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAuXYfR5PRMcJkJG6yjxw0tQvjtzRwZI/k2ks1SBgVhtCh1TcMFraq
            /u367B6E9BrGHhPZNtTcceMunC+Tow1+JIAHQPQU1+l1w+6n3esNgYUvakv0C/Dj
            opOh5mWzS81UL1r+ifXKdEs4/u561GPUdhhScxnk2lsudh0fem0Rn7yDXuGofrIo
            kAD49TLV0ZEflCQLe9/ck+qvzM8yPOnDsCZlCdCZJVpOW0Aq1cfghI6BiStVkDDU
            DaBj74m3eK0wtPJlj0flebF91VNMsmQ4XSmFZeDtdx/xOJmqzB29C7tTynuPD5FV
            zREKo5wxgvaf/J3da5K5nCP/sOBIishlYVBNZeJqwQiTze405ycdglNiYVISpYaF
            8ikv0w19E9nI3GVjwm6mYH29eKbHuEJSou5J/7lS2tlyVaGI9opGRLV+X7GLwE1D
            01uaQsyTYB7mK33broIABp5Mu/Il1+Mi3uwMKzCL/ciPMMFoSbR+zth2QoU1wRUz
            A6OK3t6w5//ufq9bKGcZ3rhU/rYzfk8nHY1F/5QBPM95WTGZZ7CjAMPzyc6Is/CL
            +7jtPZPrT05yc9HKPqG2RPWP3dziw4l1TX6NXstMzizyaayeF0yPQ6chNTqgvfFJ
            s3ABq1R8UV0LUBmdDAxeyKOOEqrqBcShHFxWmEzk95ghdT6P5XSMMCUCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "StFqqnSArvIfK07//ejbxkP3V4nnXsj8vu5km8LcM/P";
        };
      };
    };

    evo = {
      owner = config.krebs.users.mic92;
      nets = {
        retiolum = {
          aliases = [
            "evo.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAklLSqfGIsRINPjJL+ekuIiUZThHga8kmzwYWxaublKqdgLMqgRxQ
            qA/iw99LZE8IVQxfkrJessNjjGcAZ7TbXHFphJYH8Oinj61CN5NBigxxUP+bIsuz
            24P6GCmAHhfv/fHkv97z+aa82MAB8A4UyIBmNxisE1kNXvL1tEfRxRSNXo1yBeLj
            mU4KgFN532dvawcEvLcb0vWccPKkmXMlQo14RkRzcJDKRssZpAav9UEpEp74e4js
            GFZkKvGotDGl+/8kP8BdnTMsMiGGIlyFTCPWRKoJkhV03HxiineNwkYxY2Dr7kOJ
            iUNaxe9ul/wpc2OUA6mN9SYUtZ6o97gnw5enkhs649esepQDd7u3PGfz4+JRSmop
            ZKP9+WLJmqKmWywxIzZRwsVUZlOtl9dwg2y5fwR4yPHQBGO2za2QXwWecAr17NRu
            JIdqDyvTZPrIrnMa2OzQLY7KIqbaVL3PZqUtbRl2a8sep0pDZTVIUYtsWTC1uNIX
            HjHpBFcOIjEBZJKBG9C2+VO1yLJjdyBv3Mbd23gm7MW8i8VkAoLT6hH6mRr3RmDr
            FzFtzT4KU2X/VKAj+NLQXdxCVc589X6HyEhjp/gBWb4Gd2Pnqi1kQpiGR8vF76zI
            599SacfgyaCz/CL7m9HHZQRC6Ncgb139A8kdRYkk4F8x4jivA1d9NgECAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "kYWFLUnyZB+aoITG2TdgVt1hRQ4PqWTwoNiQ9Uz0NEG";
        };
      };
    };

    eva = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # eva.thalheim.io
          ip4.addr = "116.203.179.132";
          ip6.addr = "2a01:4f8:1c1a:37b2::1";
          aliases = [ "eva.i" ];
        };
        retiolum = {
          via = internet;
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
          '';
          tinc.pubkey_ed25519 = "7rbs+10zzfwOPj5RoS1i/01QXuw7uIHGOHIgsjB2fHK";
        };
      };
    };
    bernie = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet.addrs = [ "bernie.thalheim.io" ];
        retiolum = {
          via = internet;
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
          tinc.pubkey_ed25519 = "pjCpkZToBUBbjUNVMWfYJePZ6g7m7Ccr9WedfKEFsXD";
        };
      };
    };

    ryan = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # ryan.dse.in.tum.de
          ip4.addr = "131.159.102.8";
          ip6.addr = "2a09:80c0:102::8";
          aliases = [ "ryan.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "ryan.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAzvThkmiCHUWwof7UYZQ7BPTYN4ibQqO3WMig/FNalZHLBJeyyhu3
            oEvw065RyJukX4eEFySdys8JU8GWRU0EB/opquKLy1tkLokBfH+/z0/C/VhtTWUC
            S/ZqC53N7BkYuxkLsvp5fc2G1Ttm04vQUJ/dHMpxnIfw4SLEJnNBL/5/cYEtCRWn
            I4BXmvkW8K34NypVLumJuWfQ5S2NVryXe/il1GUd/TaCiBWNwZ86x9bfJuKc2/p7
            Se8PIAQaN1GdpmGIsj/2yNkxI1Y87E9DE6UJFD444IQ/fFMq3oLuAMFrRJSM2rTy
            15h5kZ5KV2nkarPZmIRADTp/tzoMdJmh5wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "saaS/b0GU3s268Move4i4mxNmxWACTiOvdGY6TpT0uH";
        };
      };
    };

    graham = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # graham.dse.in.tum.de
          ip4.addr = "131.159.102.9";
          ip6.addr = "2a09:80c0:102::9";
          aliases = [ "graham.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "graham.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAs+UnyCIjHAGiecv6lFHVRw9pLuHfqmIlyKToQChJKvsMEh1G5KpO
            KdvLuXcCzWQlhg3Tuh2a8vAYfACeJkqHY9YgH9ZSCrP+Fn9hb7eeosspQzLbC/6S
            xq4QXSLgnGmwolS5TV4FgaxPgi/cKCAfT+Az9yXyVYWX3xmyfgPULUDiFdoTXBZ+
            H2M+tDhzd9mNIZYHNcHPE7hccRETFhM80w1R9w1eyb4MkZ4XA3FBZ8hm4L7/XSNJ
            EtDiBnB3q1HwlfGzcju+TLCHal7Z+wZJ8u/1iNmhieGxzg5tQTuhMa1bn/uaqyct
            bdqnXGk6UcOUlxV8gdJh1I4M6MF8DXH4hwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "WOyyqCmTWMqjakFBnUoxsJv9o/XvdIJT9UHt6JfRuQM";
        };
      };
    };

    mickey = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # mickey.dse.in.tum.de
          ip4.addr = "131.159.102.10";
          ip6.addr = "2a09:80c0:102::10";
          aliases = [ "mickey.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "mickey.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAx2EgoPlsOZ+2+R2Gdn6E2QK1Jo+v4g16M+PqqBYtc0vno8uFmBZu
            q2S6UuHFtS3VfUL0I1+ZYcOWkNuMrL6PYBdu3MeK2GGW8IB4RSzoba7QIcz2W6ls
            YuyLbsBKJafMzNi2E+7fTrkJRezrcbS8IUhHuKYgDSHH81yXquaJhgGid7dtZPFD
            KHA4Y2l9zOGGyiFi2XfkvDES7uAAnBzOgTZzH5iR40qRqZhH4Tj5iowKvZZw7/A5
            G+v7v1YXXYW6ROsfEtMlxns921nkeZIlMaAofoaV2Qtf757GGcg6UjJHH9X6mwSb
            165IRVWSikZjkbdeg4Zz3dYkHtfFhNQM7QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "Pc/H50xDTU3dUxYynclf/Nxg2Q5k1aSdwGv4SBQPYCO";
        };
      };
    };

    astrid = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # astrid.dse.in.tum.de
          ip4.addr = "131.159.102.11";
          ip6.addr = "2a09:80c0:102::11";
          aliases = [ "astrid.i" ];
        };
        retiolum = {
          aliases = [ "astrid.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEArYO78+rLxDYBxt1MZ4VDjdSvoxJ8/De5R+/Yo0Uh1vJJtlkQUfAK
            o2uOQvX76Y1EByAu1hMKsquDJrmnEQKyaBVUv1xkU9kQPxDoUkHdQaMoyjjCLKHV
            7OjRRQ+PCAjCVfaIR4P0pXGXShBYVqITdr8R/fH7f+M6I+s/H4KTo9zpRY9YUzXV
            V6t3PCTMBXWxa2kNTSTe1zpGHccOd3FWs6r+0DZ2bPg/6Qh/VszQI7NXRqgSLNgi
            J1+PaO0h9IfICNYYaWg1r9gh6nd52U9S2B6eipizrdWdyHuufWzn52liNztSEe9g
            5VC0PPAZFIxEkhoAP/HGTnNvXLOo960IXwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "I0rk5Co9QEuyramaxNSI2Rq43qgRF2tJr5Lf8nlBjUO";
        };
      };
    };
    dan = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # dan.dse.in.tum.de
          ip4.addr = "131.159.102.12";
          ip6.addr = "2a09:80c0:102::12";
          aliases = [ "dan.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "dan.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAoZq+Nt1H+tcLRVE5LiJXyAItRIcIJNVeFenN54era7Yr0+OE7s14
            r19N7g8cb3ytgbxb1P0t8Dd2ziKUdEoOdVK7/dqx6oM1lwjOBy4rtcGmy6hHGRhT
            +Si6NxHnQVkswzL2/4DcBxg+D40GDIz0QlNhT7TC3TW6gtKbTopHMZoC3cyw1iXF
            iZB8HEv22Daq+/gFt9rcV2cRhdsDIX3TEAu+wXhDMtB9V78CzgOHV7IBrlnQHTeq
            3TmcQ+AHKZB3nY5cUDvbSUWHOrG4CQ0w2pf034s7l3AMLClXcr0IORZtCgEhCmE5
            tgg9Y6vKH2S0a25naf5rOFCvEXt8TZF9lCc42hfKCJo/LE2LoqKluAMUWgXUtv6s
            Od6AcV5RW3QkgRiDi6niPVVAnDGSUfqUNaJhmBzlfD6PzwBpPlcODf5dk/H/FhzZ
            nGpG4lptvknrBZxz9Vdyv3a/CE9VA5FbgDdOJMk5fbNG6XH4BoESjKQ/tHwvDRwO
            Xz11V5MQYk4aYq++AgkoyCgw37rWqgR2WE/X9tV63qUAiBHJoZ48QPmqrZwEt8LC
            92eTKbxfl2iroqs5vBqKiXcRAWgXwO25rb+4CJUfD8b9AdAlm4unoCcoYluJ9rO1
            5xs2x/b09U3YXkMgO67cju+Vg68ROnihokH+5pyfuMMsHSAANC+uWD8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "bEWH72WNDGtn6uGy1h1m3T8rH2pHoL8zNU1ADq4TW+L";
        };
      };
    };

    jackson = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # jackson.dse.in.tum.de
          ip4.addr = "131.159.102.3";
          ip6.addr = "2a09:80c0:102::3";
          aliases = [ "jackson.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "jackson.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA11g2uc9+tw1Bmvs4U6gsyimZ1hR1rnWTJw8CDRw6t7gx0HhRnRgB
            0Fv11KjFGbImrcZ1wxCOJA+RQise18YnlpWIwYf/nDPGR21a1wcg3ZxVRa3/UQzb
            pvbTFqK9NpPt7ENWs25ZLnatTFfc+D3kfoUSrwIVGKAaM87GlMkTH3FsARo4xj6H
            NJeQ2snOIbLQ3xXQm3oZ5YN4CN30mtrcae+jIMlKnagBuDjP9UZ73GLHu1gJAPb6
            iXdBK6/UTGY9uYKNHXnpf11I4rPSPK1r/6KxS1sX65hh4BT5Fs5goRhVqIeeICVJ
            Ufnwj7se3Ao24nLXyTRmVXaTEVN/AqPyyQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "cjYmx4Bb0JjogXNov+wSiPPIHM06n1jQnPJbP/E1yRN";
        };
      };
    };
    adelaide = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # adelaide.dse.in.tum.de
          ip4.addr = "131.159.102.13";
          ip6.addr = "2a09:80c0:102::13";
          aliases = [ "adelaide.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "adelaide.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAp17cmCeFBu+WLKuhQQmYy3iVm/Vd42T7WA+WPaMDpejpf4hNFl8D
            MYtLjEo44oOHKE95UK+CfEKjvY+XIYgr/TfXPXPbTfeUNlhwy/anK9Aek4tX/V3z
            dkS139Tp9ffDq8jUkiITaIXBpMzWC8Pc+hvAUwOyq80YII2Xp+K7+vhpdXKP6Zo0
            eFd15nCWBhx2LBxnFSE+JT/bpuC4GdGhzAsafjnoR9Jl8kJ/wjIhI/b3j4l6udFq
            Pn+/1z8mmb2LGkTg4cEUDWd86CCtkYVQW5/E0fHWFzUWStl/f1hEOENU4Cqy7GaD
            ytioO8RI0ENZOdHZiy6vFnhPFG5Er2t4jQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "FBuLCjr31Z8ijUNAgzMHeuzyKUP9zvHLijtQKBouxPO";
        };
      };
    };
    christina = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # christina.dse.in.tum.de
          ip4.addr = "131.159.102.14";
          ip6.addr = "2a09:80c0:102::14";
          aliases = [ "christina.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "christina.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA2pP2uCuvFWKfUwYIOcmPkqG8q/rNNyfw9C39tIC0VsDD6WJ0I7xZ
            S57AiG4j6OZwbv7/89qAR94SM4Q9LgmVHhUHf06gPhO0MTVNt0N1PrOnfxeCOlgK
            aH6DWZVhiwwiYwAJ55CVNFSkuL7/FtJAkAXmP0Y+xmn0mi1GpMa6RfSazqNPkXik
            HdB7u96D7Ul75yPdTDi0dvMvCxQGo4PQBctheNPY61s7P1/7tRhBT+22iJn3v2Bc
            Q9qLa6WuGIuFYCxT7GDGrKVu+V9VhFJe42p8yyIscqFagc61C/whN6v7eOh22gSR
            8EbaexJIQ5xl8ZIJto+fr8MvYAQR4FpopwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "/W10YGvcWZnVxCB6pxsC2D5A7QMk1Aw81YWi1p0Ex9O";
        };
      };
    };
    wilfred = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # wilfred.dse.in.tum.de
          ip4.addr = "131.159.102.15";
          ip6.addr = "2a09:80c0:102::15";
          aliases = [ "wilfred.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "wilfred.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAtQMC/LTfesg49VU06TFt3ikA9cdXuWzvg1FkCo6yXBlQt8fLFYg1
            YBthh7W6CZYJwf6lGXUBtCiOnxidoQ89Fq61AQGGYzW8G/vqHwFaPGHQ42Eru+XG
            RDi7ZjqOibdTemahX7gcDk6irB9WbkuXIS15n5FdQHhmjiun3zaEMoWpyiSM1HFQ
            UfJLI8pRtB65RTdT3yp8XMASldTAlSlFj2AYlDZkgcRYLZuVWb/Wz7EewG4SiB9T
            wZ1pUIhdnGNdb37S/VIjMPavaV4HK7u7awqwaaIRUYwWloCo8LGRDCXa3iEMql7J
            tI4iMEsmjHi8P5mQp4ZwwNLWucdM9+Wt6QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "5dMnJmUDmCnN/3dc7fTPlXweMmbs6W+VgSgZi+p+u2B";
        };
      };
    };
    river = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # river.dse.in.tum.de
          ip4.addr = "131.159.102.16";
          ip6.addr = "2a09:80c0:102::16";
          aliases = [ "river.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "river.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAumAOQf8/nv2Ml082e7fgoicoQ7stspsx8v1bkoubW9yq/LbdcTki
            cP6uZkLBXgd9cPq4vhSso/kaGn6UzPWeUCLv17gPn6jFlB1AYN6mJNLOxJyotajc
            78SyqzDyZ4iA0W44w98ic5yWQ/uaF3q3npFlHtAN4fD0aw50uR+2TgH5zCB8iNul
            CNIRWU9sp1t6VFYSN49tcqZ4j6jb6q/MyH5o5WLPasnq0SzQd09rnhUax7QLbIBP
            iNBYLOFYMYmoN1WlPwZf1Dt6NeFGyNn5aWE3xtOG6FYwzzCEXYOxxdXzuL5Mmiv9
            6d71+XWEAzNT9OQB3oGCLAamT+9le7SqnwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "gcgEqGyhO4S7Q44vID/cpbbVaLSNYov97oGwa3pSI4N";
        };
      };
    };
    jack = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # river.dse.in.tum.de
          ip4.addr = "131.159.102.17";
          ip6.addr = "2a09:80c0:102::17";
          aliases = [ "jack.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [
            "jack.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAx5ROeRs0CxD2IPfkrFB4O03/Kf8Ajjmd/J1gpCvHROKL+gDvaCO1
            68RbTdC1f5K6Dwc51oX87XisrD435aNCqnlE8fw1X8ZYLKRlZmLDBJGSupm35jj3
            w80L9OTK07I3cK6AqCfRWyI9Ja6J9PGOT95h8OtiAPlYssEkSqGJrtwe61V5rq1A
            st7khZO0b+xYsr+ZgOVuZMDAco9DwG7NFfpWkzAgTF6q0a+kdjFrFb6SaeJJf12t
            WUGWEkaTP5iLQ/h3M+a7MH0Col+aPNEWE4ycqZR9U3E4pMqoD7tjkYdYtv9kR/j+
            ZtN2Vlw+hKC47DA3zRNRcg7DC+FFDNFaYwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "vmtUoblzicabsNFu7u889cF9pgBKoJpHHIhY8Y/eQgC";
        };
      };
    };

    blob64 = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # of2.dse.cit.tum.de.
          ip4.addr = "131.159.38.25";
          ip6.addr = "2a09:80c0:38::25";
          aliases = [ "blob64.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "blob64.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAsl8LfS/l8zhkF9wqUTXndGZovIdIeZXeH/AZ3VopHn2yMn7HN3sy
            sM+p0ypXgV02h8faWgQsKzbhZI1XNl8vK5jo0snb9wO0qTiIViSeVfcGJN3rMvsW
            FmgcoVX7Juf3RD+oHbBc9CM7+vRbk6aIKyr3zRbGF1Ge9x/N2HSqjhYYKZ74JzJf
            kTbN/t05gvzYcQCa6ueR1K+jysALC2SCbRNXMLDQtgMc9Jv+oPJfxxCxZUJR2/M6
            E/+sfbJ+oOl/EviXzM/HH14sOeO1v1xbw0ih75BWAOC1zvrIPg/Cr3y+RmDsK53K
            eWa+2bvT7quaBLsVh9N51RSORUlXKdd2lwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "m6YO0REcHjSORwOJCUBLciavYTNewcbxdt2TJnGz9xE";
        };
      };
    };

    ruby = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # ruby.dse.cit.tum.de.
          ip4.addr = "131.159.38.70";
          ip6.addr = "2a09:80c0:38::70";
          aliases = [ "ruby.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "ruby.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAzqrguDMHqYyidLxbz3jsQS3JVNCy0HaN6wprT1Ge1Anf5E8KtuXh
            M9IjYPShzzJ162rYaJdd2lBmc5o435j+0/Gg5pySILni9bILhuRr7TMWN0sjNbgr
            x0JRbpMmpW5DOmQx1BSyA+LLNbyVVnCc1XI0P2EaRr1ZrRSU0bpE/7kJ//Zt7ATu
            GfqJTuL2aqap12VMKAfjRByyXA9V7szJMRom2Ia3cWSXhie1E0OOvCNT+InKXx4c
            QbEGX71noCgsNgxbD8AVSwMnNV15vdnbgwK/1QzA0Cep1uxFS05TXJZLZTjcGwG0
            Kp0kEjntq1rCqgdoUHIubNB17efU/oP6aSrdfvtgeYBjn0zSLHSUYdhf3JHd1Fvf
            Ov2TwHxt/sm8d91UjhrkYwjf2nzSruAklYDnIDJiHgLFoT5WuOoVlnfUjRpQEw44
            kp8KXsd24Y0UT5XJO5cQA+kZ1vl2ktHbQGTqYuYDB2FKEnBR/JIwJzJfugcGiyRx
            OukQ2/rjnS60JA2pHUEfoezIAMhYAF+EPgOgMcNSSRYUVBpPVKD26oGTrNn0AtnO
            ALW1vqUDwxb0cpv877vN1VfqvLE8n8Zgtt7itdT0+vxNPxICvF6//LNYUeDoQ3pj
            w+1ZSdYZsvIQ7tDcilnL0hU5/nfsSIbHV+ceuLde1xDt5c7Tnl4v/U0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "TV9byzSblknvqdUjQCwjgLmA8qCB4Tnl/DSd2mbsZTJ";
        };
      };
    };

    amy = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # amy.dse.in.tum.de
          ip4.addr = "131.159.102.20";
          ip6.addr = "2a09:80c0:102::20";
          aliases = [ "amy.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "amy.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEApa/qv4uKxr8lpQQau3dqgNqtXOtnN/u/5VlF2f/oNt+cDUAmAgaq
            6ktUv5HT27xCye1lJ2XNaXDF1lSUNgpdhmv7lnqqwDYi3m8HBnBMUlgXuT1mFtFv
            RybbrvbedKka4+MMXsUmFJj1udOzJSBfRIVO+M4lRvyWUbm2R18mnrz6DK9++EmL
            JCTOcBZYzjCa7OciBPJfjLrLAZZC9JnRxrvAnF2tMzGZiaCI4uX5ZKUMeMO/pwBD
            13MhxdDJeXOl98+nKRBZzft9K0qZmAnfR1a9a0dS6hstUWvl1xDLQP3L+/89sjee
            PjchaS9qQxdjj6USCEqMJOyetWzN3rabSwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "a4tdZ49nmEgYqhW11FDPhV+Oj2IFsOV2PSjxgJlceeH";
        };
      };
    };
    rose = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # clara.dse.in.tum.de
          ip4.addr = "131.159.102.21";
          ip6.addr = "2a09:80c0:102::21";
          aliases = [ "rose.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "rose.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAtinCwGjGfFTdpU+CnugM4MH6z4XLFlLMIW4Z642iq2arXOrrCq09
            yRG5UC6CBqORnF2FZhHu+wJQMexLXNILavyG6JXERvCm6S5MaFc2YlHSyBcV4AqE
            Zrjb1wSvlXGcom8C6/HGElsHqI9ULtiUqEEzES6UgUVcO7QrEy03264KZ0y4M/Ov
            5CpXbyg6tRl3CoLJE+eXyLdOGwHo/eN7M+YSaTU6bEYjJGYAltnJDO9DZxtnaZn2
            qSImJEwRD7YMPvs/zf/kKI6ihaF/oQMWyj/f0Ik/eif7rd3DRBlWFaZYr+JQBRZg
            jkaQ6EEX94WKHv6RgI25dqh8hpMaoY0OYQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "GZyz9AVjJlbE34pS2zURwVBZCCzpD0S+VqToLlB5aBI";
        };
      };
    };

    donna = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # clara.dse.in.tum.de
          ip4.addr = "131.159.38.222";
          ip6.addr = "2a09:80c0:38::222";
          aliases = [ "donna.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "donna.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAs34lPq8SnVdzMdPkWQMfeM061Yh95wqqGOdGODiyoWdsP0ErRH3/
            HjgmB7luMl7MdL3ZKIpZe/IR2OSAL+6HBE/JPIapO2e1DFFEg42AI58lgjrR0yEr
            Q59ZeGu+V95l+jC08IUoS9K6SVTkDCVe2b4Akf5oMtHAAG+ELtzh2zrPH6lkrXYd
            LvzIWcrmqu1AnmmUiHT1JleCDfSn2m/ev+LcY109lN7LCFA5VL12/EP2FhM3ELHq
            j2gAdvD1LAKq4var2MnR0MnKg0k1vMGSgwK+hj0AoLNiYivo8bxoRBNbUb94o4jQ
            8xfbYyAFxpxdi/bFDmT1UjkouJ1Y8I8GJwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "8XlFDxQoGq6Fr40PTDyF8GUwP2+YSDp8By0vlKn1OlO";
        };
      };
    };

    clara = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # clara.dse.in.tum.de
          ip4.addr = "131.159.102.22";
          ip6.addr = "2a09:80c0:102::22";
          aliases = [ "clara.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "clara.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAqebCzWDLcSU0uSA54Ublw8JSM5kErwJlOr2QOFVm0/QPWNDDqoV4
            rquS25NRZ37c4hj3BuINQrItAy7pOVrp0SARXZdyqMz3VoGndDge6p/8KEuRFQZi
            nmYrnsSuys0HOLfb2xQkOkGKBwyEc1hNGHFcw8XtJJMZSFBchQp1C8o3B3uXZq7j
            yBdUAh0crLPbL+B/xzZPDdLMigh922ejuPuGhtrTKOIQ1Jhyi5ft/Xif5JJja1Ru
            i/FUxzy/PBz+h7X3yTv4DOIyuMYMJQZpsUGBj7cwueab6rgxyV8upHLdZQ/2YI7m
            Q6cFnskLkLGlnR/gXcamgj3Sa7J3HQX9TwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "WjXoFt4TG0SqCewYXyH563MACWxhjDixCv1Dk8mDe9B";
        };
      };
    };

    vislor = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # vislor.dos.cit.tum.de
          ip4.addr = "131.159.102.25";
          ip6.addr = "2a09:80c0:102::25";
          aliases = [ "vislor.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "vislor.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAzMOrwiMFgDbITQEnXBJev4bSprV2Hg04xuEUmdoMJB4OJdBrWY7G
            71aHXtAjBqJqRYbvSoRPa+jQcpqRHNdNctfE1wq3nUkOYSM0OHGoFwb3kfybh+vu
            flmAY75ZlVRz3srITjMADpHeiuAEOmGPmlbLiUY09I2qjcaSzYYsTiGnyWSp95tL
            g3CRqiC4kj4fM0B7lCp/dz/iXDvqWEgoGEQH34x4xIIToA+DkHX5/2NAl4aaiq9m
            JQ8YCz5qBox3nD6W6bwwsEyG4vOHNcCLHBdVLEbfUFHM8XDjF3dJZ+RjCYxdiEjM
            dZUckPeLf/8XDkNMZm1eKMIJBvcH3UESLQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "ZMFZ4fd75fh2OLg/SuiTsavs013E2tUaCDqX76LPI6K";
        };
      };
    };

    tegan = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # tegan.dos.cit.tum.de
          ip4.addr = "131.159.38.2";
          ip6.addr = "2a09:80c0:38::2";
          aliases = [ "tegan.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "tegan.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA0Rg6R8RZkQ8lYMegQDOy8OB6y31jfcVufBHx/QWFzzX/aaoyJDNo
            QPVwn7qWnArqXvvwMYT6boPLJPmEFfxt9mwHX3lTEQKj82FT86hjG5axujIx1EAa
            5v1A5UbZiBwntQPhHP+ULaQLPhwRfU+NftCpf3NC1frP0xd4U0fzdmO86jVp9Mwr
            T07IvJzaSg1tIodT8F4CYktRCC9u8uQmtOrU/8FoPcWHMFxYMLR+qB3KQ3WF4EPU
            qw3vtADATjDpagFr5+E12dc//04iTJc9sQ254WcLH9kfb+i+AFJ9G8H24iM9Iyal
            YuEAYPV1XOQ8SZ9PNrKcSJxvMebzOkhvmQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "7AlLnzY96NqA9I0og/RaY+HekMSUGlxoDFNy5VRzE+L";
        };
      };
    };

    xavier = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # xavier.dos.cit.tum.de
          ip4.addr = "131.159.102.27";
          ip6.addr = "2a09:80c0:102::27";
          aliases = [ "xavier.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "xavier.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA4mAnaYD/tImIFmpDVK819M8XqY5j6i85E1FuBfqkx4FFRY4qHlpT
            1ykybkPPI64y0B2Nr698vg52LQB9jclbo8sm5q8X/kkgwOM7jS9uwO2k8XnUqi3l
            bV22v5xTXjqPklP71Bc5dlvjVUfX5KwkEpzrSQ+NqIPrlzs9/nIeQONVdhrEsRBk
            kMaSGHzRcBBP+rbsi+GqIN4VKUKlUmP3XiDGj9yGUnTSDbMf0CoOksmle9i6s2rV
            J5Kp78LhIZ5el7Gd+wMtwWQYKcHbZJwpFt4ODlefKknLeDV0uJgJpUmF0r32Hlj0
            OAkmtpaRdweKj/Va31jNvhgJidDe/qgrFwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "CkupwxlJG2SH5FkqxathLC4PSZCsrDMpOTOtvCqid3A";
        };
      };
    };

    #vicky = {
    #  owner = config.krebs.users.mic92;
    #  nets = rec {
    #    internet = {
    #      ip4.addr = "";
    #      ip6.addr = "";
    #      aliases = [ "vicky.i" ];
    #    };
    #    retiolum = {
    #      via = internet;
    #      aliases = [ "vicky.r" ];
    #      tinc.pubkey = ''
    #      '';
    #      tinc.pubkey_ed25519 = "";
    #    };
    #  };
    #};

    irene = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # irene.dos.cit.tum.de
          ip4.addr = "131.159.102.28";
          ip6.addr = "2a09:80c0:102::28";
          aliases = [ "irene.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "irene.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA2P7QtdfRmrI1gDBKQJv+kAJBeNz5/bDuJfCcLJ6fkOt3MduZvLDt
            qCj2LBhdXBAEcIKzrvzgR9r9mfEU38hr6JgO01xBvyEHg9YSJJ/5iGu17ZdyeS96
            3RRpUs2WtepA3vrhSH7xgncXXC3xoercdUGs2GK3qlx9VAcAR0hy5teQmkBGJecm
            C2wXwwEajOL4HAAvv7gyEp6S5ow1DHSJXxe32FQb18YnR2WHXh40RfWJnvs+zeaV
            QYN4v6XNJKGC3hux1QyGQbfABUeEnNzl9bDmVICpcNPAy09VWX10UCymmWX6612L
            EYXB0lt3PhcsvRcXNlV8ZZrukY8EU+UY1wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "CAyCg6zgO14v5oI4sV9dpwmjS7jy9Ttb4SiuKI5OQyN";
        };
      };
    };

    ace = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # irene.dos.cit.tum.de
          ip4.addr = "131.159.38.181";
          ip6.addr = "2a09:80c0:38::181";
          aliases = [ "ace.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "ace.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAkkvRfeBjdD8Z+QNe3ASYXKQucV2riD3Fw5iOYMIIriGlcRSKa0bc
            tP2vO7kjB6xkxuOgG+WYBj2fpW+1FM2xXI6FSSmajUUibizPqFB6tofADUVsqZmP
            /CF5+hR+vm8Q4FBfpbZ3KlJ5htCUV+khCL3oFPMFrzheMFUhgNxuu5i9lC7r52D9
            9c0nQ5iKkdIVCuVMOy5/k+E9Qfp2+KqwIp2MqihIe9/EfRGvkiVLqFrqCBAVtLQc
            XS+Gbu1QaZqdoN9GKa+3m10H+WuP6V7xDfTonkZKs0W2onFKOdmrzH8/0Uvq1bw0
            PTk1wnJLyJGpQI0Z1qGbvipoqIoWStbD2wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';

          tinc.pubkey_ed25519 = "FlD70xZzsNbSVdf02AxuXJnC0S/S/Rogd4H/1t4edQF";
        };
      };
    };

    ian = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # ian.dos.cit.tum.de
          ip4.addr = "131.159.102.30";
          ip6.addr = "2a09:80c0:102::30";
          aliases = [ "ian.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "ian.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAyfm4Bqg7SOHjXl6PRUVwdDAsYigj7aOYhL7lPKoEADUYIXO0/d+Y
            t6KzKGV0Lvz0cKMiY4sGVJ8KGvEb1VhfKLSUdqCPY5spT44zbpvyrOSck6aGOB50
            Fu1EEtGj9uPtJorJx0UtjMi4WPEUs5Xg1YxxncmFB8pFfQcqIVUh7l5lPiCB2Udq
            P+GPwnewTDTJvqCN7Rc6zW8WKjxJseNXkFF/hdZuNPSKE+fvQdkrfDmYt8KbWVV+
            0LlsCxT4rLkIhvqEXMA6wELtnGAw8PflBV3tFmBBRTG64Bj7Sni7QI359QL+5sBo
            rXgOTExX37JnSPIXtAmKm6r3+eexZwOB4wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "pjccEHqFQ0eq+TvTFm9mjTkisS7aOMVimi5wMGsgwnB";
        };
      };
    };

    bill = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # bill.dse.in.tum.de
          ip4.addr = "131.159.102.1";
          ip6.addr = "2a09:80c0:102::1";
          aliases = [ "bill.i" ];
        };
        retiolum = {
          via = internet;
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
          '';
          tinc.pubkey_ed25519 = "bN+knMGCqK+HkdOucynEXxeqGFOS2u8oWLRDV/gNIZI";
        };
      };
    };

    eliza = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # eliza.dos.cit.tum.de
          ip4.addr = "131.159.102.34";
          ip6.addr = "2a09:80c0:102::34";
          aliases = [ "eliza.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "eliza.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA7mHnj/MsBXzNEOmBpofWqS3vrU7h4GDoHyN2SiQmp4h7alE3MWOq
            1nxdb/FbUu7NvpvIOnvT0G6H8O+LABSAoRkE5Yhwm38lYmz7axNxoUefhz1RgPsT
            QYuyba9vkxoz6aJvtiDtuDJF7g54PdDdhkinIs1hbUu+aee8x/TtCBv2hAnJEe1L
            Z8zVFlGM20XxCodtvriLvjnQqMuSwnss0/bDd9bOvS6mZAQuNHBAiOU8aZUfGIuo
            yTmW0Iy6AhomWSS6BCuM6xtmucUYrFl43gFtQGXFaRVWZ4pPXgJALbd+yOk9y5LI
            KwUReXdCBD6Xh7BxbZwkzKuafK09nzhFLQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "WA7H6UiAd0Os/zNv2Qb3zur9iqpaVnUAFSB9JZWsegE";
        };
      };
    };

    jamie = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # jamie.dos.cit.tum.de
          ip4.addr = "131.159.102.32";
          ip6.addr = "2a09:80c0:102::32";
          aliases = [ "jamie.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "jamie.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAuIh3Ryqjkwe5fqG3Gc/A1pNRF3prSOYGT93FeNtszeu/z7pI6ET8
            JvMgNptkdCH32qq623/xmG8HZbFlSLNkHWBe6NKmWdXCfsjjNMmMHc7JpiF3hT3p
            23KPz/iYKsKTjjnzHYLX469P9+FIBBETNYkoFSWe0qe9D550+fdROB7z1insZKv1
            lgFG5z8SzZWwuN+6fv1cbHiy9fJY+R3FFp2WdmS6SsUx+IoN2HLOUEB4YC88ibDD
            i/sHaovFRRkvS8nvagq82Z6U1mkcrj3pKQ0c0BzzwVre9FeGLQf2akRq4WPfav/w
            ToYzCv2LSt/6vVtjtb46iemrk1sETP5c+QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "C+SzIWznqfFBylhYZgbVxJiEb18uXhE/p+4PY7n9+/P";
        };
      };
    };

    joy = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # joy.dos.cit.tum.de
          ip4.addr = "131.159.38.93";
          ip6.addr = "2a09:80c0:38::93";
          aliases = [ "joy.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "joy.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA67v1AxGRE+PJOjil5M1hOhtjlH6UD/BYUzhmOzVp/syLqlVYzzaJ
            rwONmLhk6UBowCPJ8ydsLP+GHEGWMoWPWNCKS+rReE/DgM3V7ECaAedTLR+QBIFO
            w6Wg19ABU733AfDpl9/IwzFHVMiUnIfFLEil1QxZXZ1B9mSvn0Jq0d884H5xYeUr
            0PT6Ed0fLoGD/85A+PAbt3fL83oydfvaVpB6696kEJ+pyAFtDMXQ18s9u4lwSPlX
            JG8W2WwRvPSMpC90CZ9muyEU5arGFMCdctrlO03kpM+QJQfTz8XnbxVPKzW/GGBu
            sWSfzXzdQlbIaVh7NzuvgXNTv1KQguxpLwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "1mKqlu77a3/RldwNkekvoxzwUQ5mDjwpMTT0uuPLadN";
        };
      };
    };

    martha = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # martha.dos.cit.tum.de
          ip4.addr = "131.159.102.33";
          ip6.addr = "2a09:80c0:102::33";
          aliases = [ "martha.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "martha.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA1DZxJUJ+M7BfgfHnYO7lvkRjjtUHHQ6+SZy7DcuMoeY9NlsOeAOo
            d3oVMjtlt9tqP3eMZCh+ru2FAlrRfYnYslwBh7JC5RqBlB4BpoVxyly6S7DAEx+U
            UIp80hV2Ny+C8I+/ogb6R4ZemAVvQHQPTfHE+RbFDYhLd8+9ma/LQ4kUwJWCvCiH
            yoTpIlQnTfD/oI4Gfwb8CnauJ00txT59ronsG4fstJQ/PUmLA1vhlOeKlJbx1bx9
            OW4j7Uic0wRI/YxpP6SutzZDKpcsJZT3bIOTsd3zIFssU5iL1QDk4SaPemSOmTi+
            GylqOLWdnlFX2apckkTjDQWI5gHIcPMyxwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "iC2EdFTSgwOXoC5HSaO4G9o2DNhVvg3PEv8N+QsjYFF";
        };
      };
    };

    vicki = {
      owner = config.krebs.users.mic92;
      nets = rec {
        internet = {
          # vicki.dos.cit.tum.de
          ip4.addr = "131.159.38.185";
          ip6.addr = "2a09:80c0:38::185";
          aliases = [ "vicki.i" ];
        };
        retiolum = {
          via = internet;
          aliases = [ "vicki.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAu0Ds+Ymx9fJOdJbVxyUyuHjRBs5c/bDghN8Cfk7Yknp7rg/AjzUe
            2A3JzZwLN7x6z8yglBjo8hZbyqIpu6M8QfX2MhEgNLa41483K2I8iIQmF0krflCa
            ywjXFitrziCCZVwDOmdmc1V/1U6/9oJWoMDAM05+ErTNCR9EbKEiXUcYjtFgfqIL
            u/AFL6KTtkf7T/GRbRWEqchTXRAXD5X3VUgLgnokcdpQpwadRjKIAG8G97FiVTvC
            KrmE4lORWAzppJATzTNI0sQpL+ORDD1OpJwHZl7riLaoBxlk/47UkZOMerP9H3af
            Gj/Nv47ThmnIsjW6I1tI5vNn3si7InvyUwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "+Hdpt0X5pWMlPkdKTU7Oqs1qEccRz2YswLcaXhNkurN";
        };
      };
    };
  };
  users = {
    mic92 = {
      mail = "joerg@thalheim.io";
      pubkey = builtins.readFile ./ssh/mic92.pub;
    };
  };
}
