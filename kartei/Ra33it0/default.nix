{ config, lib, ... }: let
  slib = import ../../lib/pure.nix { inherit lib; };
in {
  users.Ra33it0 = {
    mail = "Ra33it0@posteo.net";
  };
  hosts.DUMMYHOST = {
    owner = config.krebs.users.Ra33it0;
    nets.retiolum = {
      aliases = [ "Ra33it0.Ra33it0.r" ];
      ip6.addr = (slib.krebs.genipv6 "retiolum" "Ra33it0" { hostName = "unispore"; }).address;
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
MIICCgKCAgEA6Cb+b+snYpsQv1J0yMPSL4P0iKs2EkDtqtt6kBOvqFTr2lRB2thp
mu9fRbz/CFmcvFXoEMWQEEkKcyhgJEola2+7Ra49iMNX55o/I0iZ499ZI5rIK/JG
+A60ijPCh5TSGYIMiD7VWRsxoAtzB1DZ6n4z94KN0wQB5dXKuLPjk/TDfJPuzMrS
J5k9uSyBKcRdW2iop78wNOnYO8NVd9wr6odUBc/L5J0krDU2gLGRGJGDfoW4zfly
5DwtY58DBCZS7uFAymKBdvEBUzj7/wD0B2Jfq/EUOdEKeFbP2G4fdOTQBuXGDqMi
dqufCy2cK3AOi5l3VaC2LfkCMztRBPzryY8+EcfjgqENBPCx55GBZDrtn/W+29S7
ynMfI+1e8TntpFGLhuJXyl9//rG68tvYUED5MQ98OXViiffW7lBo7i5TCck3f9Cv
CWYM/HzSffzztK8bF0DwhdWzjtNcwZ05XfA2krGZyMj9UxpwN84o1syCnnYC1Xzg
4r48fUhubXXE4SbdnN68pCNCct9DT8exPeYeJL2FHi6s+EsfBY+NGEAaQGJTeQEW
zUSnX/txoZV6xGUKZ4iOgfQ4MBCVVdtPAaurNP/esVwOr0WF0DTuBDPGBaOqo+Us
Ef5cREwrCE8nEY8tu3xl4M9iuCTwBuT79YFhfNI3jr1lcg6f8wGaTYsCAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "cFCAfLbDYv/Ty3m34aHgHr1dXGp2DSwfP0K7GG1TA7D";
    };
  };
}
