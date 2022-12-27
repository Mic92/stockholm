{ r6, w6, ... }:
{
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.2";
      ip6.addr = r6 "dea7";
      aliases = [
        "mors.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAsj1PCibKOfF68gmFQ+wwyfhUWpqKqpznrJX1dZ+daae7l7nBHvsE
          H0QwkiMmk3aZy1beq3quM6gX13aT+/wMfWnLyuvT11T5C9JEf/IS91STpM2BRN+R
          +P/DhbuDcW4UsdEe6uwQDGEJbXRN5ZA7GI0bmcYcwHJ9SQmW5v7P9Z3oZ+09hMD+
          1cZ3HkPN7weSdMLMPpUpmzCsI92cXGW0xRC4iBEt1ZeBwjkLCRsBFBGcUMuKWwVa
          9sovca0q3DUar+kikEKVrVy26rZUlGuBLobMetDGioSawWkRSxVlfZvTHjAK5JzU
          O6y6hj0yQ1sp6W2JjU8ntDHf63aM71dB9QIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
        pubkey_ed25519 = "kuh0cP/HjGOQ+NafR3zjmqp+RAnA59F4CgtzENj9/MM";
      };
    };
    wiregrill = {
      ip6.addr = w6 "dea7";
      aliases = [
        "mors.w"
      ];
      wireguard.pubkey = "FkcxMathQzJYwuJBli/nibh0C0kHe9/T2xU0za3J3SQ=";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINAMPlIG+6u75GJ3kvsPF6OoIZsU+u8ZQ+rdviv5fNMD";
  syncthing.id = "ZPRS57K-YK32ROQ-7A6MRAV-VOYXQ3I-CQCXISZ-C5PCV2A-GSFLG3I-K7UGGAH";
}
