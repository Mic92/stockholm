{
  ci = true;
  nets = {
    retiolum = {
      ip4.addr = "10.243.13.39";
      aliases = [
        "au.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEApD+HJS5gANbZScCMLxgZZgHZUsQUDlyWTLNdANfo0gXQdsYRVE/z
        9zMG/VE9xwy0OC9JM73YaEymXdmWa3kGXP2jjQnOZyJTFMNFHc8dkl+RBnWv8eZm
        PzFN84ZjnYXyOpXJFajR8eelzqlFvD+2WKsXAD5xaW5EmCBTMIjB/zSuLBpqnIHb
        PqQA1XUye69dQRjjcPn1mtYQPS78H8ClJjnhS76owFzyzNZjri1tr2xi2oevnVJG
        cnYNggZHz3Kg3btJQ3VtDKGLJTzHvvMcn2JfPrePR2+KK0/KbMitpYAS687Ikb83
        jjB+eZgXq5g81vc1116bA5yqcT2UNdOPWwIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "bfDtJbxusBdosE6dMED32Yc6ZeYI3RFyXryQr7heZpO";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBsqDuhGJpjpqNv4QmjoOhcODObrPyY3GHLvtVkgXV0g root@au";
}
