{ r6, w6, ... }:
{
  cores = 1;
  ci = false;
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.113";
      ip6.addr = r6 "113";
      aliases = [
        "massulus.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN PUBLIC KEY-----
          MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEApwYalnJ2E1e3WOttPCpt
          ypNm2adUXS/pejcbF68oRvgv6NRMOKVkoFVEzdnCLYTkYkwcpGd+oRO91F+ekZrN
          ndEoicuzHNyG6NTXfW3Sjj9Au/NoAVwOJxAztzXMBAsH5pi4PSiqIQZC4l6cyv2K
          zUNm1LvW5Z5/W0J5XCUw3/B4Py7V/HjW9Yxe8MCaCVVP2kF5SwjmfQ+Yp+8csvU3
          F30xFjcTJjjWUPSkubgxtsfkrbbjzdMZhKldi3l9LhbYWD8O4bUTrTau/Emaaf6e
          v5paVh9Kczwg7Ugk9Co3GL4tKOE2I7kRQV2Rg0M5NcRBUwfxkl6JTI2PmY0fNmYd
          kdLQ1fKlFOrkyHuPBjZET1UniomlLpdycyyZii+YWLoQNj4JlFl8nAlPbqkiy8EF
          LcHvB2VfdjjyBY25TtYPjFzFsEYKd8HQ7djs8rvJvmhu4tLDD6NaOqJPWMo7I7rW
          EavQWZd+CELCJNN8eJhYWIGpnq+BI00FKayUAX+OSObYCHD1AikiiIaSjfDCrCJb
          KVDj/uczOjxHk6TUVbepFA7C8EAxZ01sgHtUDkIfvcDMs4DGn88PmjPW+V/4MfKl
          oqT7aVv6BYJdSK63rH3Iw+qTvdtzj+vcoO+HmRt2I2Be4ZPSeDrt+riaLycrVF00
          yFmvsQgi48/0ZSwaVGR8lFUCAwEAAQ==
          -----END PUBLIC KEY-----
        '';
        pubkey_ed25519 = "QwKNyv97Q2/fmPrVkgbGIhDTVW+uKu+F2enGCtZJgkM";
        port = 1655;
      };
    };
    wiregrill = {
      ip6.addr = w6 "113";
      aliases = [
        "massulus.w"
      ];
      wireguard.pubkey = ''
        4wXpuDBEJS8J1bxS4paz/eZP1MuMfgHDCvOPn4TYtHQ=
      '';
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKH8lFXZ/d2NtqyrpslTGRNBR7FJZCJ6i3UPy0LDl9t7 ";
  syncthing.id = "R2EGJ5S-PQMETUP-C2UGXQG-A6VP7TB-NGSN3MV-C7OGSWT-SZ34L3X-H6IF6AQ";
}
