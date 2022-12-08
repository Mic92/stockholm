{ config, ... }: let
  lib = import ../../lib;
in {
  users.oxzi = {
    mail = "post@0x21.biz";
  };
  hosts.ancha = {
    owner = config.krebs.users.oxzi;
    nets.retiolum = {
      aliases = [ "ancha.oxzi.r" ];
      ip4.addr = "10.243.32.1";
      ip6.addr = (lib.krebs.genipv6 "retiolum" "oxzi" { hostName = "ancha"; }).address;
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEA5RSP7nWZ1c04kvQBxoHqcdRKpJuRDzD3f0Nl2KhS7QsAqHJGdK7T
        RrsoZcvJaKIFnlohJ4T1YpGGcXqShhTmKt3sm/0awLhD+zTE8lAlvEj+lnCkHls8
        eXO+VDB5FelibW/wEnvdImxKBaSVt4RLmMyTuzS9xklEq8Q+wMvzJktnV3pWJjYX
        /JBYQEUHlrqXldBlKGHkU1KhFZHD/wzV5Ybkku4w1BHrMUHJNwHpTshD/QBDiJFj
        iRA3e3Jfpp3qj2uWetGuP7NlFpZCh/fSrTqkAE8uShcFlplbgJIEGz2pp644maqw
        XxRWPH1Iy5NHwVz/GSzQ67vsEunRJjueFQk8gxnhjh/CAlmE9VdxfGQOkejBAq+X
        zCbqyflLPPz3Qx56TVpmAOY4gma7sfsaYAv+zv2paUxFKBfZrEL5UNoIevV9kZDn
        nDixTQ6cDxHt3yCVzvwqTTBktZ0mYom43lvKSUnihDrQL1u338labFPtsZTOK4bo
        687ToSUC6u80VcnMTZxPFYOgTMjdCZPo+j1bhzmCQQCzcStRSeKRta+LOYb73Tjz
        M6CwC9uaHDxhtmysXpZ4Qp83tfU6h/AsBJJpBdpkyLYXTq+E32pIq6RtKFFQL00O
        /e0DzUzSB30oKLW1i2ZxWRQMVqvNdKsyq4glI4eRjnRmrnXOwTb7Y2MCAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "oLvC/Y3jfGH4a8mBbv9eCCWKsx32gDGW/iCyia/fuBD";
    };
  };
}
