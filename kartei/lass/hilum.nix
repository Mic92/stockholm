{ r6, w6, ... }:
{
  consul = false;
  cores = 1;
  nets = {
    retiolum = {
      ip4.addr = "10.243.20.123";
      ip6.addr = r6 "005b";
      aliases = [
        "hilum.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN PUBLIC KEY-----
          MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAul1zLdJ76kIqVWjxT2bb
          pLx6gu6VycxaDcWAoTWSjPsOT2IJf3NYC6i8D6WASnRqR6djp06OG7Onu0r5hZhi
          V5nelDUvR75qVAx9ZeuQDSdNpWuVMds/C3cQM6QQHD1kFwnr2n6VH/qy0W9duW8c
          SGX3C80nRpmY0cCEEnxFdFdLSd0c15M+lFVAaqh2225ujXyyvkwH874yvpWLPSdh
          4xjZdrOFarl5yb9q83HcZsdunn+469BeKCWB8bs+nRsp9Wwj1en1yAZTB3WazYNE
          saFQ0xGa7VGfHN0PjqgZEF2I2IiQJ+H3N5XRQ7dcJzsDRB8lMrCx2ynJkJRSjLXz
          vgZjW+Rf47V9CLRjJGCp1xh6GbXqjsIYh5yqZkgH4Sm1VpMBYdr/kLjiygwzV8jY
          8uoBUgEHLc5B73/D3GlMe3bOJmxxMfyPITVTFHgznycalBNBSsgKpIwWae6LbYhZ
          wrpi66IQOyC6YYThqn8pz3KUz17HxyacA/mS6/jcRP+IiHb9CYcS4BsjTpH3NnM3
          RkSWE3FGE+ULH1W/VeA8pZRKAR1rypvMRdewbFTQpe/dNgif5O5Fe/7l/6KDzzCh
          Zqqr6sEFhutPUd6PcaVtQlfzYkJ9MGYWYr4S17D7Q9V0H37a0AcRaYH59FCmlFjl
          87b8jfJNXlKFW+EBxBxN2uECAwEAAQ==
          -----END PUBLIC KEY-----
        '';
        pubkey_ed25519 = "9D50r3DmftSe2L++jPktQRbcCrE4sEazMewgbQbodRH";
      };
    };
    wiregrill = {
      ip6.addr = w6 "005b";
      aliases = [
        "hilum.w"
      ];
      wireguard.pubkey = ''
        0DRcCDR0O+UqV07DsGfS4On+6YaZ3LPfvni9u1NZNhw=
      '';
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPARXXe0HaP1r0pLqtInhnbYSZsP0g4VC6aaWP7qi5+w";
  syncthing.id = "J6PHKTS-2JG5NOL-H5ZWOF6-6L6ENA7-L4RO6DV-BQHU7YL-CHOLDCC-S5YX3AC";
}
