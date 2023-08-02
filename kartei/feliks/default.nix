{ config, lib, ... }: let
  inherit (lib) flip mapAttrs optionalAttrs recursiveUpdate;
  slib = import ../../lib/pure.nix { inherit lib; };
  hostDefaults = hostName: host: flip recursiveUpdate host ({
    owner = config.krebs.users.feliks;
    ci = false;
    external = true;
    monitoring = false;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum.ip6.addr =
      (slib.krebs.genipv6 "retiolum" "external" { inherit hostName; }).address;
  } // optionalAttrs (host.nets?wiregrill) {
    nets.wiregrill.ip6.addr =
      (slib.krebs.genipv6 "wiregrill" "external" { inherit hostName; }).address;
  });
in {
  users.feliks = {
    mail = "feliks@flipdot.org";
  };
  hosts = mapAttrs hostDefaults {
    papawhakaaro = {
      nets = {
        retiolum = {
          ip4.addr = "10.243.10.243";
          aliases = [ "papawhakaaro.r" "tp.feliks.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA4bd0lVUVlzFmM8TuH77C5VctcK4lkw02LbMVQDJ5U+Ww075nNahw
            oRHqPgJRwfGW0Tgu/1s5czZ2tAFU3lXoOSBYldAspM3KRZ4DKQsFrL9B0oWarGsK
            sUgsuOJprlX4mkfj/eBNINqTqf2kVIH+p43VENQ9ioKmc+qJKm4xfRONRLp871GV
            5jmIvRvQ6JP0RtNd2KpNLaeplzx8M61D9PBOAZkNYAUTpBs4LZBNJj4eFnXBugrz
            GkBjmm3Rk7olz0uOZzbeTc6Slv2tgtN5FrQifdy4XIlsKcBTzMkYHEZstmldJgd9
            pGvfmem6uPcXrF+eDJzqUn0ArH7eOIS4F0+DzugJz4qX+ytvE4ag7r2Vx0Pa9TCY
            hpn0lqwW+ly1clM0SKt59v1nQ4oRW4UIbAZaIgp4UJbb3IGSwbq7NuadvHpNICHi
            4pqQD+1sSEbGLAZ0bFjLIYFg9zzNjLeAxXpn49WHOEyRlq3h+SUQcG2EuVMI28DX
            lILKSoOJsuQupURPubaxkiNEa5neYk9hZ8CWgwSG/VlyRLuNsVDVn2dBma43Mr10
            LHMkX2/a9t7ghokugvV2XMP9Es9A9TGFShM9UtFAlovdad+SQ8FBPNheDwIhjCJe
            l5NIrMrmQIveq7QJ1szxYhqfl1ifU0c+YxeMkg3tvEuQV/tk/oki/aECAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "5G49yQPjkkoGZxM6CeDy87y6tB/abtelUAk55wJ4GpP";
        };
      };
    };
    iti = {
      nets = {
        retiolum = {
          ip4.addr = "10.243.10.244";
          aliases = [ "iti.r" "ltd.feliks.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA5TXEmw3F3lCekITBPW8QYF1ciKHN8RSi47k1vW+jXb6gdWcVo5KL
            Ithq3T2+jWJJQoOJEDl5Tvo9ilF0oE0AqSNnvfgS/t8xfFVEsNvHodbonXXku5cF
            N7oFooAgQRXAUJpEQLtcfx9kJutSYgGeEvoRGZkWaqY6tzPL45U2WEna+MJ/P1Cd
            57JMOLeJJEjZKtC/XqPOQ81KNcm161RKekHas5ZNK30QEVP9QsjTDoLesYwm1ywt
            4LiHRHSSHd65pKXJvi1haEYw25BxIun7kY4IQHrfEuK3DNs0kyYJj2rKL4C9kHgT
            hYd+fFl1i/X1BjPzo+ZY91ahLVX3UPpOsB8vC9Q7Ctm1Nkc/bCfKRUNbamkS0Bwf
            tngak3heGvuek6Y7qWQUkvMkPLhZwZUXUz+DBXGWXabP5LL8Z/y3V+Qqj0snEsZ3
            9iOF+eeDw2/9hBzRzBPGtwL1DREgd+1J/XlHLcjF4jzkMhweIXw2Yh0Jq7D5Nqf3
            kPF9n/50zbQneSGEiKFeHm1ykag/KV0ebWHUOy1Gydbs7+RxT9GUiZofI6kyjJUI
            g1w1ajkZYIIqhIvhMHudLay5h4kLkdGN9yuRNO/BG5sGk5MywZHyMploIX0ZRVui
            +H3Sx2y268r/Fs6JcaddmzFwFqNmdRTRv/KBp91QGnjcaJDzQPKg/IsCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "uG9D7hrWNx+9otDFlZ8Yi31L6xxC7dzGlqXBLkzJCwE";
        };
      };
    };
    tumaukainga = {
      nets = {
        retiolum = {
          ip4.addr = "10.243.10.245";
          aliases = [ "tumaukainga.r" "hs.feliks.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAj1q28QzUlag0i+2ZEpZyQEbrtuODj6pCCt2IX1Uz1B83outO2l/n
            2g096QLhCT/h5QwPtoa5YihYrANH1wWc/RKjgVLfVdcdHzZvge2Z9UE22XbH5G5O
            D3AmGFDfuZa9KEmqoDqUnuBVvctywHkbf2bqye9Cje2DqXe9h0EJhR2agVwwi41Z
            I2EGLSI6/oA+ySymVhP1dDzr+keWVIbxfbBtRO2OFWg5IQ/H+d3ZP4yUumu7rJsN
            h+mBOdLL8PACGDKRj76Gp2+1raXYyfsnn9OI4+sFiQ0F/J2FjbXser1O6z6fRwY4
            s9R4vLtkUXix2YjCOzjAIBHsR9UtC+tYpzmlGyY51CNGNXoklWUsfS6ZIKyPKRwJ
            +Bsi/zZCqTT1BKC5X9dQU7C46JHHRAwn8EUE3r5dja4rHAlU064ow/1EGrbWzjIj
            LyZ+e5vMLIc99T3g2nlF6x2Vo9L5hYiUVej+qk4i869JNxZRQ5fTG8HJDxf67Wvj
            K+By7fN6XaTiN28E1PLL6562Vj4sJUiFdkGVyW3bdw4IYaqKiabyjT6TZ33AK+VC
            V4jB5VplSo8QVv73OjkS8Iaicrdcb8YuhIKnpIStwxeaLc8KGnstOfFIJCoFuXfb
            ktTeB0OBZ/bZpSRlzGI7tMX+7coYFqr7uv3wB+/D3Wck47vcxz9woxMCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "li5wJeMjS+fNAMjrn3KVxZby/kGfH6ZoWArYSGMFAQD";
        };
      };
    };
    ahuatangata = {
      nets.wiregrill = {
        ip4.addr = "10.244.10.246";
        aliases = [
          "ahuatangata.w"
          "ndrd.feliks.w"
        ];
        wireguard.pubkey = "QPDGBEYJ1znqUdjy6JWZJ+cqPMcU67dHlOX5beTM6TA=";
      };
    };
  };
}
