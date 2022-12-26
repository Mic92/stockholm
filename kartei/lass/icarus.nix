{ r6, w6, ... }:
{
  cores = 2;
  nets = rec {
    retiolum = {
      ip4.addr = "10.243.133.114";
      ip6.addr = r6 "1205";
      aliases = [
        "icarus.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAydCY+IWzF8DocCNzPiUM+xccbiDTWS/+r2le812+O4r+sUojXuzr
          Q4CeN+pi2SZHEOiRm3jO8sOkGlv4I1WGs/nOu5Beb4/8wFH6wbm4cqXTqH/qFwCK
          7+9Bke8TUaoDj9E4ol9eyOx6u8Cto3ZRAUi6m1ilrfs1szFGS5ZX7mxI73uhki6t
          k6Zb5sa9G8WLcLPIN7tk3Nd0kofd/smwxSN0mXoTgbAf1DZ3Fnkgox/M5VnwpPW7
          zLzbWNFyLIgDGbQ5vZBlJW7c4O0KrMlftvEQ80GeZXaKNt6UK7LSAQ4Njn+8sXTt
          gl0Dx29bSPU3L8udj0Vu6ul7CiQ5bZzUCQIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
        pubkey_ed25519 = "vUc/ynOlNqB7a+sr0BmfdRv0dATtGZTjsU2qL2yGInK";
      };
    };
    wiregrill = {
      ip6.addr = w6 "1205";
      aliases = [
        "icarus.w"
      ];
      wireguard.pubkey = "mVe3YdlWOlVF5+YD5vgNha3s03dv6elmNVsARtPLXQQ=";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOPgQIMYiyD4/Co+nlOQWEzCKssemOEXAY/lbIZZaMhj";
  syncthing.id = "7V75LMM-MIFCAIZ-TAWR3AI-OXONVZR-TEW4GBK-URKPPN4-PQFG653-LGHPDQ4";
}
