{ config, lib, ... }: let
  slib = import ../../lib/pure.nix { inherit lib; };
in {
  users.samularity = {
    mail = "samularity@murks.r";
  };
  hosts.murks = {
    owner = config.krebs.users.samularity;
    nets.retiolum = {
      aliases = [ "murks.r" ];
      ip6.addr = (slib.krebs.genipv6 "retiolum" "samularity" { hostName = "murks"; }).address;
      ip4.addr = "10.243.0.42";
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEAsooKgG5zkt4lPY7ROxnwuGuDTfN9YZPsKHnENjmzvMkfdwdM3aGW
        2fkZfn8Q8pQ0eJaTJ8/Lavk1LKOB5pgkuloyOtPfpmJY++LAn37rNxnQvUWP3kS4
        JJ78whRJqczO4b3TEh2Dm/DsvH3E/Gpwq5TXiRzbjQ9hspZlPptRkda0UveAh1fn
        wFVBtUEOmQZ3cpGCAdo2sMcBoZwJXSQAm4z2P+eTXXo2IIjWgS8zQu5nvX/y4RG8
        D+HSe/X58yVRww4ABU4g7cNzzqLVKFEAuznlmpiZZ6EMGZ7xbZQlDmJuAIHIXrn+
        JVc1Yo/WmFYbN/6CmIUHE80/434Zg1dK87honUXizeDO2cyIb1vEG432Eqd3TsG7
        3YjJOHqaiNqBd60tFrT87mjbuCiD6jKlA5AQtzSSdDhq8G0i0Il8Do6SnpY2ACx2
        CvF6s3sWXP3YtY/AupB+okPoqvC+96Oxs2KJwQSatQlDr0aarror19NKC6b/Y4qo
        C0tcDa7VZiOB+rM6iIjRiM3QEyWEBia3nj2n5T98DgPooqmSF2p4NKYcaBMSv3f5
        aD2oZzdBqZnPAbi2CNoNN9pdVOB0Ckt9DaiMaiROEeAvxL3xxwaWhMPBFbSvGhca
        ciMdqaPlgqKu7WGtmU4a0U3JXhWTjin1mroD3kduykgek4fAmvcP540CAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "4ZAQxhHBUT9KSof2tjadgMHD5kxND0AtXgjdTzs/F+L";
    };
  };
}
