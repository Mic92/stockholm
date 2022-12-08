{ config, ... }: let
  lib = import ../../lib;
in {
  users.oxzi = {
    mail = "post@0x21.biz";
  };
  hosts = {
    ancha = {
      owner = config.krebs.users.oxzi;
      nets.retiolum = {
        aliases = [
          "ancha.oxzi.r"
          "gosh.r"
        ];
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
    marohu = {
      owner = config.krebs.users.oxzi;
      nets.retiolum = {
        aliases = [
          "marohu.oxzi.r"
        ];
        ip4.addr = "10.243.32.2";
        ip6.addr = (lib.krebs.genipv6 "retiolum" "oxzi" { hostName = "marohu"; }).address;
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIICCgKCAgEAxHLkvuH9JMXay/fEmoWTEqLHg9A50EzkxPVBn4nyezgp5vxsUqJz
          Ys0VnO6jjgz9T6N8u0CaavsqFy+X48A/+uB5nd/nGDZNaeTg+HUdznT4OFAJEaDi
          uQREDsR5ZwmpI534eESHMdn5LSb/+5CUgj2xsoOBxnukALm1YccPxR0PPibCm+Z1
          P8r+1+nBgIPv+cknTXzhWMF/L7UOXuyV3Jmk1BIhwYmzWVes6idtIyJwoCbssoQT
          cl21Czvhwx63o/QEa81qKeCK3AAAnMbp1tAxnzl7Wr/cSoBYRgSIZkOQPEUNHvpF
          fT9UzZ3DZyAOMWNjqiK1M93VruFYer05qO3jGgumDey/9gLjP6GMjBw9jVDNY9yn
          8mOKz9dkrP3v/A96Uqp+w/lYO87YrxA+h9BYY4jyPngGh0DoXddHLHAKco39vbq8
          4vQRsK5QNgquF7O9aBDMSrFosk1VFedpZQwC2LaXcjtI3aMq3vIURTbuWkutAjAd
          p9a5dRa62pWk41n6yLmalCkqnHoqVUaft9wZIxbcrDLUso7QxY6kFhjADSijnr5B
          HrBXJhNLGVjBD/W++l2CJ+L4njmy4eGrOTBvIzosCMbtgMtfuu7WSQhsjxTwclbD
          utT3hmgxDPZydsvzRMsLNvNQwUoiLAL4mz27V9hYcJTKPAbUL3y8h48CAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "q/DIqHkb/8Qu7OrCXaBeuxkT9XNPmxo8uF3GkFFC6rJ";
      };
    };
  };
}
