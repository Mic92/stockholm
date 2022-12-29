{
  ci = true;
  nets = {
    retiolum = {
      ip4.addr = "10.243.13.41";
      aliases = [
        "hu.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEAwj5T9Rejp8zGVrHjqA+OeMvcVpax4VazssnRPSUznUEOdVEeSJL5
        8gDBJPtIfxF8iunXr5K7CW036tKvYaGMDwYMOPJZXhFCmU2yUF2g4BcqEhuDdIfO
        +D2Pfr4lc9xO90SKOgwJ53qhf5yqeU/WQ3dpCF/n8k4SUmdafTsvh00UrxYpHuTU
        C22BRXIKR4r/sCJUitWQSWNdSQUxh3lu7sUPr+6sZyJov+eu8oBVlPgYOv6u9nZe
        YhrbCPDKMGPfnQTAtWfHIxNt70Ec5AG6ddQzLeVcM2gP5qi957Fert+C2RNtbz5s
        Brbw1bqZ3P+CGzvxVJZtirvR2f3HkidGPQIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "PV8Dz9ni2cPXyJGiG5oU0XWdJkUPgrMzDuzHj7kpMzO";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO+Rrf9tvuusYlnSZwUiHS4O+AhrpVZ/6n7peSRKojTc root@hu";
}
