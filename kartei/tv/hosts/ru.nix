{
  nets = {
    retiolum = {
      ip4.addr = "10.243.13.42";
      aliases = [
        "ru.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEAr4xgpXPr/OGrLO5vwur35esesbAwREwShGJf9btt65UQXst090tD
        GWev8Yfi3Mr241r1TG7zpW3Idh5nth2yhzVvqGc9m6QmK27v2MKpb+ppjOKab7RL
        1KfdBAwjdrWdL2xO3XAYOUljxWoIV4VKX8kEBvjJEDOwl/u+g5mB3yLWebtIT7Wk
        EneMU6wvCVKhOPeqyXmbqO/+j6+bqxkKP2/5hHcX3a91+15YbR3SvREK2rUm9stx
        Rc3kmGUO/DiGK6MmUmt+qieGo/4vheK8hij57dY0uXFIC7U680QzV7jsUmtlKGBL
        PoK/Xn6TLLG6nozgmF+q8esYyaYQFrwU2QIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "Eg9l+RxFSNrQ9RkTd8tSkoTIG2m7zhQpjUJBWJRft1J";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcNClgsey79WzdEQs/8qkLMHzc1SCU/MqyMerPcUi8X root@ru";
}
