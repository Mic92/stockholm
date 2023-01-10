{ r6, w6, ... }:
{
  nets = rec {
    internet = {
      ip4 = rec {
        addr = "157.90.232.92";
        prefix = "${addr}/32";
      };
      aliases = [
        "dishfire.i"
      ];
      ssh.port = 45621;
    };
    retiolum = {
      via = internet;
      ip4.addr = "10.243.133.99";
      ip6.addr = r6 "d15f:1233";
      aliases = [
        "dishfire.r"
        "grafana.lass.r"
        "prometheus.lass.r"
        "alert.lass.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAwKi49fN+0s5Cze6JThM7f7lj4da27PSJ/3w3tDFPvtQco11ksNLs
          Xd3qPaQIgmcNVCR06aexae3bBeTx9y3qHvKqZVE1nCtRlRyqy1LVKSj15J1D7yz7
          uS6u/BSZiCzmdZwu3Fq5qqoK0nfzWe/NKEDWNa5l4Mz/BZQyI/hbOpn6UfFD0LpK
          R4jzc9Dbk/IFNAvwb5yrgEYtwBzlXzeDvHW2JcPq3qQjK2byQYNiIyV3g0GHppEd
          vDbIPDFhTn3Hv5zz/lX+/We8izzRge7MEd+Vn9Jwb5NAzwDsOHl6ExpqASv9H49U
          HwgPw5pstabyrsDWXybSYUb+8LcZf+unGwIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
        pubkey_ed25519 = "P+bhzhgTNdohWdec//t/e+8cI7zUOsS+Kq/AOtineAO";
      };
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGv0JMp0y+E5433GRSFKVK3cQmP0AAlS9aH9fk49yFxy";
}
