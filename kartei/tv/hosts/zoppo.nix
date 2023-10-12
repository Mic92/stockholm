{
  nets = {
    retiolum = {
      ip4.addr = "10.243.13.43";
      aliases = [
        "zoppo.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEAtMzW+RL4mik0JHLL2p21bWy81bVv84mi/NKHyewXxx9EahD3cyOP
        D/2gLipJnzyjxvM7HzR0FQpBCD5VLmRk2MrRENUGYzrNFhSu36gVg5bGngr0pZld
        Fosd5wemGx6SUoXMzuBT/st0sg1Azg5mtA1joQTr47NlcwfFrUe7YT8i67szxJSO
        Qo6BP+Zl6uVr9m8zSVwrj7TAoC7DQLhCTb3QJdpyYibHwpo0bMyyJjeuL/NpnOpH
        Bv1pZ0oXr/nzCo+8jyQY4L2NonJnHtlzYOQf1cHJ+v+P1jOgGgBGqjo0LOE29Tvx
        cfQYGLLZc4mRjVQJF3NYW6kHFqKmuu/y5QIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "nVIMNp7r21MYneV24x6qVesCjQYM/pvF6nzaihH7GfD";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMk5DVtgzKmbJTsJs81GIMYE3YblnJJTc/FtVukKJK4J root@zoppo";
  syncthing.id = "F4GDV3I-QX6QAA5-32MXHXE-2RJDYBO-RFXGDFR-EGMN4IQ-OJDKL62-NCUWOAQ";
}
