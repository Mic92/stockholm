{ config, ... }:

{
  imports = [
    ../../tv/3modules/identity.nix
  ];
  tv.identity = {
    enable = true;
    search = "retiolum";
    hosts = {
      cloudkrebs = {
        cores = 1;
        dc = "lass"; #dc = "cac";
        nets = rec {
          internet = {
            addrs4 = ["104.167.113.104"];
            aliases = [
              "cloudkrebs.internet"
            ];
          };
          retiolum = {
            via = internet;
            addrs4 = ["10.243.206.102"];
            addrs6 = ["42:941e:2816:35f4:5c5e:206b:3f0b:f762"];
            aliases = [
              "cloudkrebs.retiolum"
              "cgit.cloudkrebs.retiolum"
              "habsys.de"
              "pixelpocket.de"
              "karlaskop.de"
              "ubikmedia.de"
              "apanowicz.de"
              "aidsballs.de"
            ];
            tinc.pubkey = ''
              -----BEGIN RSA PUBLIC KEY-----
              MIIBCgKCAQEAttUygCu7G6lIA9y+9rfTpLKIy2UgNDglUVoKZYLs8JPjtAtQVbtA
              OcWwwPc8ijLQvwJWa8e/shqSzSIrtOe+HJbRGdXLdBLtOuLKpz+ZFHcS+95RS5aF
              QTehg+QY7pvhbrrwKX936tkMR568suTQG6C8qNC/5jWYO/wIxFMhnQ2iRRKQOq1v
              3aGGPC16KeXKVioY9KoV98S3n1rZW1JK07CIsZU4qb5txtLlW6FplJ7UmhVku1WC
              sgOOj9yi6Zk1t8R2Pwv9gxa3Hc270voj5U+I2hgLV/LjheE8yhQgYHEA4vXerPdO
              TGSATlSmMtE2NYGrKsLM7pKn286aSpXinwIDAQAB
              -----END RSA PUBLIC KEY-----
            '';
          };
        };
      };
    };
  };
}
