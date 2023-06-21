{ config, lib, ... }: let
  slib = import ../../lib/pure.nix { inherit lib; };
in {
  users.DUMMYUSER = {
    mail = "DUMMYUSER@example.ork";
  };
  hosts.DUMMYHOST = {
    owner = config.krebs.users.DUMMYUSER;
    nets.retiolum = {
      aliases = [ "DUMMYHOST.DUMMYUSER.r" ];
      ip6.addr = (slib.krebs.genipv6 "retiolum" "DUMMYUSER" { hostName = "DUMMYHOST"; }).address;
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        DUMMYTINCPUBKEYRSA
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "DUMMYTINCPUBKEYED25519";
    };
  };
}
