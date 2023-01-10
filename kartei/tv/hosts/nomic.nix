{
  ci = true;
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.110";
      aliases = [
        "nomic.r"
        "cgit.nomic.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEAwb8Yk/YRc17g2J9n960p6j4W/l559OPyuMPdGJ4DmCm3WNQtxoa+
        qTFUiDiI85BcmfqnSeddLG8zTC2XnSlIvCRMJ9oKzppFM4PX4OTAaJZVE5WyCQhw
        Kd4tHVdoQgJW5yFepmT9IUmHqkxXJ0R2W93l2eSZNOcnFvFn0ooiAlRi4zAiHClu
        5Mz80Sc2rvez+n9wtC2D06aYjP23pHYld2xighHR9SUqX1dFzgSXNSoWWCcgNp2a
        OKcM8LzxLV7MTMZFOJCJndZ77e4LsUvxhQFP6nyKZWg30PC0zufZsuN5o2xsWSlA
        Wi9sMB1AUR6mZrxgcgTFpUjbjbLQf+36CwIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "sBevGkYkcNKd39yf/Mp0whnsWIJfTGxSU1lbqN305nP";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMIHmwXHV7E9UGuk4voVCADjlLkyygqNw054jvrsPn5t root@nomic";
}
