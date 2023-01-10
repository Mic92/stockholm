{ r6, w6, ... }:
{
  nets = rec {
    retiolum = {
      ip4.addr = "10.243.133.115";
      ip6.addr = r6 "daed";
      aliases = [
        "daedalus.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIIBCgKCAQEAzlIJfYIoQGXishIQGFNOcaVoeelqy7a731FJ+VfrqeR8WURQ6D+8
          5hz7go+l3Z7IhTc/HbpGFJ5QJJNFSuSpLfZVyi+cKAUVheTivIniHFIRw37JbJ4+
          qWTlVe3uvOiZ0cA9S6LrbzqAUTLbH0JlWj36mvGIPICDr9YSEkIUKbenxjJlIpX8
          ECEBm8RU1aq3PUo/cVjmpqircynVJBbRCXZiHoxyLXNmh23d0fCPCabEYWhJhgaR
          arkYRls5A14HGMI52F3ehnhED3k0mU8/lb4OzYgk34FjuZGmyRWIfrEKnqL4Uu2w
          3pmEvswG1WYG/3+YE80C5OpCE4BUKAzYSwIDAQAB
          -----END RSA PUBLIC KEY-----
        '';
        pubkey_ed25519 = "ybmNcRLtZ0NxlxIRE3bdc2G4lLXtTGXu+iRaXMTKCNG";
      };
    };
    wiregrill = {
      ip6.addr = w6 "daed";
      aliases = [
        "daedalus.w"
      ];
      wireguard.pubkey = "ZVTTWbJfe8Oq6E6QW1qgXU91FnkuKDGJO3MF3I3gDFI=";
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAq5Ovdcsljr5dOl7+2sQNKpGpdX0SlOIuCZKEiWEp8g";
}
