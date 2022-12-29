{
  ci = true;
  nets = {
    retiolum = {
      ip4.addr = "10.243.13.37";
      aliases = [
        "wu.r"
        "cgit.wu.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEArDvU0cuBsVqTjCX2TlWL4XHSy4qSjUhjrDvUPZSKTVN7x6OENCUn
        M27g9H7j4/Jw/8IHoJLiKnXHavOoc9UJM+P9Fla/4TTVADr69UDSnLgH+wGiHcEg
        GxPkb2jt0Z8zcpD6Fusj1ATs3sssaLHTHvg1D0LylEWA3cI4WPP13v23PkyUENQT
        KpSWfR+obqDl38Q7LuFi6dH9ruyvqK+4syddrBwjPXrcNxcGL9QbDn7+foRNiWw4
        4CE5z25oGG2iWMShI7fe3ji/fMUAl7DSOOrHVVG9eMtpzy+uI8veOHrdTax4oKik
        AFGCrMIov3F0GIeu3nDlrTIZPZDTodbFKQIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "urVOEGxTkBedkpszPH0XRCRMk+Fc2U9IneYMFDqGoIB";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcJvu8JDVzObLUtlAQg9qVugthKSfitwCljuJ5liyHa";
}
