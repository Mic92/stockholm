{ r6, w6, ... }:
{
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.14";
      ip6.addr = r6 "3110";
      aliases = [
        "yellow.r"
        "jelly.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN PUBLIC KEY-----
          MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA6lHmzq8+04h3zivJmIbP
          MkYiW7KflcTWQrl/4jJ7DVFbrtS6BSSI0wIibW5ygtLrp2nYgWv1jhg7K9q8tWMY
          b6tDv/ze02ywCwStbjytW3ymSZUJlRkK2DQ4Ld7JEyKmLQIjxXYah+2P3QeUxLfU
          Uwk6vSRuTlcb94rLFOrCUDRy1cZC73ZmtdbEP2UZz3ey6beo3l/K5O4OOz+lNXgd
          OXPls4CeNm6NYhSGTBomS/zZBzGqb+4sOtLSPraNQuc75ZVpT8nFa/7tLVytWCOP
          vWglPTJOyQSygSoVwGU9I8pq8xF1aTE72hLGHprIJAGgQE9rmS9/3mbiGLVZpny6
          C6Q9t6vkYBRb+jg3WozIXdUvPP19qTEFaeb08kAuf1xhjZhirfDQjI7K6SFaDOUp
          Y/ZmCrCuaevifaXYza/lM+4qhPXmh82WD5ONOhX0Di98HBtij2lybIRUG/io4DAU
          52rrNAhRvMkUTBRlGG6LPC4q6khjuYgo9uley5BbyWWbCB1A9DUfbc6KfLUuxSwg
          zLybZs/SHgXw+pJSXNgFJTYGv1i/1YQdpnbTgW4QsEp05gb+gA9/6+IjSIJdJE3p
          DSZGcJz3gNSR1vETk8I2sSC/N8wlYXYV7wxQvSlQsehfEPrFtXM65k3RWzAAbNIJ
          Akz4E3+xLVIMqKmHaGWi0usCAwEAAQ==
          -----END PUBLIC KEY-----
        '';
        pubkey_ed25519 = "qZBhDSW6ir1/w6lOngg2feCZj9W9AfifEMlKXcOb5QE";
      };
    };
    wiregrill = {
      ip6.addr = w6 "3110";
      aliases = [
        "yellow.w"
      ];
      wireguard.pubkey = "YeWbR3mW+nOVBE7bcNSzF5fjj9ppd8OGHBJqERAUVxU=";
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC03TCO73NQZHo7NKZiVJp2iiUbe6PQP14Kg3Bnlkqje ";
}
