{ lib, ... }:

with lib;

{
  hosts = {
    bobby = {
      cores = 4;
      nets = {
        retiolum = {
          addrs4 = ["10.243.111.112"];
          addrs6 = ["42:0:0:0:0:0:111:112"];
          aliases = [
            "bobby.retiolum"
            "cgit.bobby.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA+AScnIqFdzGl+iRZTNZ7r91n/r1H4GzDsrAupUvJ4mi7nDN4eP8s
            uLvKtJp22RxfuF3Kf4KhHb8LHQ8bLLN/KDaNDXrCNBc69d7vvLsjoY+wfGLJNu4Y
            Ad/8J4r3rdb83mTA3IHb47T/70MERPBr2gF84YiG6ZoQrPQuTk4lHxaI83SOhjny
            0F0ucS/rBV6Vv9y5/756TKi1cFPSpY4X+qeWc8xWrBGJcJiiqYb8ZX2o/lkAJ5c+
            jI/VdybGFVGY9+bp4Jw5xBIo5KGuFnm8+blRmSDDl3joRneKQSx9FAu7RUwoajBu
            cEbi1529NReQzIFT6Vt22ymbHftxOiuh4QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      #ssh.privkey.path = <secrets/ssh.ed25519>;
      #ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM+7Qa51l0NSkBiaK2s8vQEoeObV3UPZyEzMxfUK/ZAO root@stro";
    };
  };
  users = {
    miefda = {
      mail = "miefda@miefda.de";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCVdNCks6mrItKHYIwgW3s+NINFhHqZtLPj3l6TJUWd93ZSuuI6P+Z/0m0G9Z4tWWaXWsOCnzMA2WOKcitBbLcaQxVypJfvmfoA5CVlh4/nf8NfvbMFkVIYPehxR7YoejfKOxPOCNC3248RiD8kqa4/5IF8qdqE+mRQUIZJXvN0jZZ+rGnYo5Z544O9JqsV+VjjOgK0Fchpxf/lC8dnBucIce7gUwi5npwsGQZgSDmRobBRFVDZag1abLFNZN2faI8uqzSlU6KRRapYV266Of7j3kmDokMan4szjP1EexmTWm+arwRiz9p0M5oKs6zofez0mOyF5ux02NB3XIhbJc8CfMjeA7PmSg4ZhghjlSjIOR+1mMIDiDVi6PNLw5atzvpyfYtpf5sWpdIpXCS0lyzIgasqW4gbAiWoFPv5A0mw0QI6UqlxQ8Pdm6R7P6yQxyknrxnvFGMQPiqgl21ssSNA9A+YRd4j0nATntzOeD1bxTZkyU4FtW++0hg3Ph6HiHLfPd9w70wPr7b0RITVnBcN2ZqIO+5NIqQYU801FCNXsTuBh0ueTsVTGJYySUGkmkHyH5spLYdr1Z5w+4W+HgbxPk40pyZJ18S0umL49igxR9NsniucFy1/jqqi0TiDIsHx6vsawFT1F2rq9ZtGaRcJL6Yfz0p+uZC5rc/nI+mMlQ== miefda@nixos";
    };
  };
}
