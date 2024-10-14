{ r6, w6, ... }:
{
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.4";
      ip6.addr = r6 "50da";
      aliases = [
        "shodan.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIICCgKCAgEAx362jbzjyKsPG4zAeZW1mgDWzaBoTz6JpJlN6ycsTLkrAAQrHiCs
          Dz6sbE0zkQUcdFFuagqwROrQU81kx663azBAlHHsMs/vkVmbQk/ilXHHOYYbkRUS
          zCfBe1JwXNPUyZ+v46IgOuvLLBfO00prcDj69sIqWdRMGAvKqYssSHuelBO3UdMl
          7r5nQ+Kc5hOqfHjf1xW7eSL3BsAA1GP/nuHkhUJN4TOKXqlywTxpcJQKI35k1gR/
          zCH53qZQ6/GHe6lHEWIjrKdzg51h7cu6UbyfpVN0zoFSY3gcFemRNKk/LI8DxVZs
          DjBQCpNVzRkrbmRIS0jTpzwSIvA7O204Z4Z7Q7ocrlFP5gKKT7M+Hk18CU0DIHwp
          e5shYBGLPAswmWJQJUyXRyMjS580+ymxw5DRIym2Ogu8w3ztSOxbcWunvLAn9I84
          U6/njQxdKHeuCYBqlO1YHOJ+qKvU4HsV3EYjwGvVzxL4XVg24KvQJ4M6QZvjLYfS
          oysx64tLBW4hYv4dTA0vLSa9/0zreNKucJRAaHYGw9rC6FZDK3b8AZiNOCSz2tWC
          I/C/sw/UgZMev66MHVuO/K6xR5hpi1tW6ONZ3ecFp4N+MS8lUOQrCQ/L6UU58Qgr
          AmAP6hM3FM1TCHEOC2jpLcUIHAdLf+xdzdp2ExPZJiMAUeV310i/dlECAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
        pubkey_ed25519 = "iuu6UcJpUu+72IywGkeGh/PpJJZ9UidbsdTR00JbFQL";
      };
    };
    wiregrill = {
      ip6.addr = w6 "50da";
      ip4.addr = "10.244.1.4";
      aliases = [
        "shodan.w"
      ];
      wireguard.pubkey = "0rI/I8FYQ3Pba7fQ9oyvtP4a54GWsPa+3zAiGIuyV30=";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC9vup68R0I+62FK+8LNtwM90V9P4ukBmU7G7d54wf4C";
  syncthing.id = "AU5RTWC-HXNMDRT-TN4ZHXY-JMQ6EQB-4ZPOZL7-AICZMCZ-LNS2XXQ-DGTI2Q6";
}
