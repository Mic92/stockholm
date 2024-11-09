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
          MIICCgKCAgEAzttBobc7FsMm3ngFeOwnY0sB/lC9Y+JHHfLlh9j8kROjBhIzu+Ky
          3OVr5Zs3iAUw4yOtMVdEQX9kTkN993qcIUriBMsPBnnQPvPX9hlLvLJ80Mputqdq
          xUmnjn29DYff56VEzAfOEYeaXX63XUovQmALIk4DvAWxzCL6yyth8IJKQDnsieHN
          QmhAgQN4/rqHzaqkdN4pcnjff3Xw2dHZd0zhnQBA6pMKuBTmu0wV1HMKWHmjNUXG
          lMtXKZ8rsJsNxo9NKKxYfMX5LNf497rZHC7iMDsNSGmMa8Rhw/By94Tax7MQ2++w
          dGg0A8ON6eyM9qcLbFgNbkslEC9ustb3bWqHZJyHRyvTJ0CnTSYoeqyDtdzAL+tg
          FBqiWbDrxUDYD4kdsIt6waPx2pmVjvO/z5njbiuLYSmrICpQkRlu3SOBXPbgouoG
          6DmwakOvpHA9pPlRUCa0koAkSM2iwaICsbsdk8KghfbjX5Kbu2b8oK1V7FKpYnKF
          lLrRJk1G3tc3JV5slsbiaV/zL/JZ8IhNY1m6DYIyLeCGKLmc844o3ZoRVPqfMpDc
          a4RSEoORv3oUAc4fWXPil+AjvTSxfRSRX6L/1STJ4HtUqSwuAinCZx4ecP/qqCv6
          KpVr4zMR/x+6o5DBCHNriW8uVnOEzuxaq1k9tIUDuawED3XodTzGKtECAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
        pubkey_ed25519 = "v3VPuvfH/2JS5aUx2C9MtYoYoBU9J5LkvUdbyabKgrL";
      };
    };
    wiregrill = {
      ip6.addr = w6 "50da";
      ip4.addr = "10.244.1.4";
      aliases = [
        "shodan.w"
      ];
      wireguard.pubkey = "J1RTYvWmWZrLe+IqOrmy+wYxGyc2j6sUjIGgM1No2AQ=";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGkp+Fw9S/Af31vUP+n24cQLzbteUYosVFmV+7RSJm18";
}
