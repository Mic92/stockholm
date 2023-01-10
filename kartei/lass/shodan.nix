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
          MIIBCgKCAQEA9bUSItw8rEu2Cm2+3IGHyRxopre9lqpFjZNG2QTnjXkZ97QlDesT
          YYZgM2lBkYcDN3/LdGaFFKrQQSGiF90oXA2wFqPuIfycx+1+TENGCzF8pExwbTd7
          ROSVnISbghXYDgr3TqkjpPmnM+piFKymMDBGhxWuy1bw1AUfvRzhQwPAvtjB4VvF
          7AVN/Z9dAZ/LLmYfYq7fL8V7PzQNvR+f5DP6+Eubx0xCuyuo63bWuGgp3pqKupx4
          xsixtMQPuqMBvOUo0SBCCPa9a+6I8dSwqAmKWM5BhmNlNCRDi37mH/m96av7SIiZ
          V29hwypVnmLoJEFiDzPMCdiH9wJNpHuHuQIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
        pubkey_ed25519 = "Ptc5VuYkRd5+zHibZwNe3DEgGHHvAk0Ul00dW1YXsrC";
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
