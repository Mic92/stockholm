{ r6, w6, ... }:
{
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.77";
      ip6.addr = r6 "b1ce";
      aliases = [
        "blue.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN PUBLIC KEY-----
          MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA28b+WMiQaWbwUPcJlacd
          QwyX4PvVm9WItPmmNy+RE2y0Mf04LxZ7RLm5+e0wPuhXXQyhZ06CNd6tjeaKfXUc
          sNeC1Vjuh1hsyYJLR5Xf/YRNJQKoaHjbkXGt+rSK7PPuCcsUPOSZSEAgHYVvcFzM
          wWE4kTDcBZeISB4+yLmPIZXhnDImRRMEurFNRiocoMmEIu/zyYVq8rnlTl972Agu
          PMGo1HqVxCouEWstRvtX5tJmV8yruRbH4tADAruLXErLLwUAx/AYDNRjY1TYYetJ
          RoaxejmZVVIvR+hWaDLkHZO89+to6wS5IVChs1anFxMNN6Chq2v8Bb2Nyy1oG/H/
          HzXxj1Rn7CN9es5Wl0UX4h9Zg+hfspoI75lQ509GLusYOyFwgmFF02eMpxgHBiWm
          khSJzPkFdYJKUKaZI0nQEGGsFJOe/Se5jj70x3Q5XEuUoQqyahAqwQIYh6uwhbuP
          49RBPHpE+ry6smhUPLTitrRsqeBU4RZRNsUAYyCbwyAH1i+K3Q5PSovgPtlHVr2N
          w+VZCzsrtOY2fxXw0e+mncrx/Qga62s4m6a/dyukA5RytA9f6bBsvSTqr7/EQTs6
          ZEBoPudk7ULNEbfjmJtBkeG7wKIlpgzVg/JaCAwMuSgVjrpIHrZmjOVvmOwB8W6J
          Ch/o7chVljAwW4JmyRnhZbMCAwEAAQ==
          -----END PUBLIC KEY-----
        '';
        pubkey_ed25519 = "vf3JzuLpEkjcwZtuJ/0M9Zjfp5ChKXvkORMXsZ4nJKL";
      };
    };
    wiregrill = {
      ip6.addr = w6 "b1ce";
      aliases = [
        "blue.w"
      ];
      wireguard.pubkey = "emftvx8v8GdoKe68MFVL53QZ187Ei0zhMmvosU1sr3U=";
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILSBxtPf8yJfzzI7/iYpoRSc/TT+zYmE/HM9XWS3MZlv";
  syncthing.id = "J2LMIPD-PBEPVKL-A3MN6NQ-KL6DZ4N-K4GGWZB-E2EPLFN-PDLVAOC-DCSZHAD";
}
