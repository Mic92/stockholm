{ config, ... }: let
  lib = import ../../lib;
in {
  users.DUMMYUSER = {
    mail = "DUMMYUSER@example.ork";
  };
  hosts.DUMMYHOST = {
    owner = config.krebs.users.DUMMYUSER;
    nets.retiolum = {
      aliases = [ "DUMMYHOST.DUMMYUSER.r" ];
      ip6.addr = (lib.krebs.genipv6 "retiolum" "DUMMYUSER" { hostName = "DUMMYHOST"; }).address;
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        DUMMYTINCPUBKEYRSA
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "DUMMYTINCPUBKEYED25519";
    };
  };
}
