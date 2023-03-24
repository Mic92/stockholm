{ config, ... }: let
  lib = import ../../lib;

  hostDefaults = hostName: host: lib.flip lib.recursiveUpdate host ({
    ci = false;
    external = true;
    monitoring = false;
  } // lib.optionalAttrs (host.nets?retiolum) {
    nets.retiolum.ip6.addr =
      (lib.krebs.genipv6 "retiolum" "external" { inherit hostName; }).address;
  });

in {
  users.srounce.mail = "samuelrounce@gmail.com";
  hosts = lib.mapAttrs hostDefaults {
    workbox = {
      owner = config.krebs.users.srounce;
      nets.retiolum = {
        aliases = [ "srounce.r" ];
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAsLQ4ZEwEbgHCh7yQvZWms6586Q3Ni+dF6pmkQdxrNb/BLlplB2Db
          sDYVMAfamXDd/jPKpoevwRaXoImxk1IHSN4ZJ2liaZNDBOJJ0CnXdJHGQ3SnV50x
          9ABAeP/lmT95lhWuCg0qS0xY60ax3EDhTVYk51dPwwoBNE6dwO/lJr6vgtKBI7fV
          /IgoF7L8G7AEPz61vS0sCHld537bEPfZ3Us4gUC6/V+xKnkBRDuCgFxemOjNdu0F
          cqEyxwrPcQoFtOt9ZDF8817qdFsRQsgM8LsnDti8mhxDR/0deTYARkWIoRjLU+p9
          XLCdDBEGsbl4Bb6tYT9jz9RCNNJeXogE2wIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "E9bMsS3w72hIjp2fYicwgpdwSJDUQbh2dHEDk6F5JdO";
      };
    };
  };
}
