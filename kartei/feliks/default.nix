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
            MIICCgKCAgEApPx0Xa5tms6t9yOqrdBuz5JVheIqntIF4XK04gXMYr/lcqWj35Sj
            jM0fObbB0MXz1Di0DsWT5ukVMpvRfespif2FsRfpUOBzVQymlcFfn7D4t2qUa0nH
            AHuvoSqb2qV8YUIvVRNdnNSv1NWlbEpcsKXzg36O2ESdro64vSM5hAVw8Djo8Eoh
            AGlZVi1tplVs+DPlsMjUqjCrGeq81V7SiLwaVc7adcx/cNvzDA23axkUosm/X2fN
            Ug8UWXHt3SgH/BtTwWIpT48anIdPbkttH0d4ICzt0g3nX6+zmVhdzIjHWNsmjxaQ
            qKn2DfC1TcYffE4k4E2yENwLoTkJn3U3cCowt7OTLfNvexRyFj5E/O1Aa1VdwX68
            MTpF89Hv8SKUSMRsbyG/vFAoh/I88Y4lDis+TtBKPs1VLBtsQy1mZaIooSTslPf+
            pcUDBBUsf2/SudwvbBC1XHl1YDnRFBZG74ApVIXeIo5G8Cfm4LasppYqPJ7YzTKp
            6yoR9iKaXONTwQ3xhlBcfpMxObZTE1v8kF9sy3t9Pl8Px9f4PSbuQpp82MJrRJQC
            FYTMkUh0PZwbw7vzqDLjeW715YWeNKW6PSFT0TtY8UTNNKFslhUfuBBLGyjsU+T3
            9m9uNNhRxFoFmlKYziFzyEVWgMl67Eg0CQAulP0q9zv3d4367il6SK8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "8g19LVFwgtdpFPcqTM/pdCzWhy3ins9+LPjHIjwNFvA";
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
        aliases = [ "ahuatangata" "ndrd.feliks.r" ];
        wireguard.pubkey = "QPDGBEYJ1znqUdjy6JWZJ+cqPMcU67dHlOX5beTM6TA=";
      };
    };
  };
}
