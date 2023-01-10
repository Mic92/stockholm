{ r6, w6, ... }:
{
  nets = rec {
    retiolum = {
      ip4.addr = "10.243.133.116";
      ip6.addr = r6 "5ce7";
      aliases = [
        "skynet.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEArNpBoTs7MoaZq2edGJLYUjmoLa5ZtXhOFBHjS1KtQ3hMtWkcqpYX
          Ic457utOSGxTE+90yXXez2DD9llJMMyd+O06lHJ7CxtbJGBNr3jwoUZVCdBuuo5B
          p9XfhXU9l9fUsbc1+a/cDjPBhQv8Uqmc6tOX+52H1aqZsa4W50c9Dv5vjsHgxCB0
          yiUd2MrKptCQTdmMM9Mf0XWKPPOuwpHpxaomlrpUz07LisFVGGHCflOvj5PAy8Da
          NC+AfNgR/76yfuYWcv4NPo9acjD9AIftS2c0tD3szyHBCGaYK/atKzIoBbFbOtMb
          mwG3B0X3UdphkqGDGsvT+66Kcv2jnKwL0wIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
        pubkey_ed25519 = "9s7eB16k7eAtHyneffTCmYR7s3mRpJqpVVjSPGaVKKN";
      };
    };
    wiregrill = {
      ip6.addr = w6 "5ce7";
      aliases = [
        "skynet.w"
      ];
      wireguard.pubkey = "pt9a6nP+YPqxnSskcM9NqRmAmFzbO5bE7wzViFFonnU=";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEB/MmASvx3i09DY1xFVM5jOhZRZA8rMRqtf8bCIkC+t";
  syncthing.id = "KWGPAHH-H53Y2WL-SDAUVQE-7PMYRVP-6Q2INYB-FL535EO-HIE7425-ZCNP7A3";
}
