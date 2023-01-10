{
  binary-cache = {
    pubkey = "xu-1:pYRENvaxZqGeImwLA9qHmRwHV4jfKaYx4u1VcZ31x0s=";
  };
  ci = true;
  nets = {
    retiolum = {
      ip4.addr = "10.243.13.38";
      aliases = [
        "xu.r"
        "cgit.xu.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEAl3l7IWbfbkVgaJFM3s9g2UCh2rmqoTba16Of7NNWMj05L/hIkUsQ
        uc43/QzidWh/4gEaq5MQ7JpLyzVBQYRJkNlPRF/Z07KdLBskAZCjDYdYue9BrziX
        8s2Irs2+FNbCK2LqtrPhbcXQJvixsk6vjl2OBpWTDUcDEsk+D1YQilxdtyUzCUkw
        mmRo/mzNsLZsYlSgZ6El/ZLkRdtexAzGxJ0DrukpDR0uqXXkp7jUaxRCZ+Cwanvj
        4I1Hu5aHzWB7KJ1SIvpX3a4f+mun1gh3TPqWP5PUqJok1PSuScz6P2UGaLZZyH63
        4o+9nGJPuzb9bpMVRaVGtKXd39jwY7mbqwIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "xYgYM9rXS73RFKUHF3ekQWhcWzuBLOPYG2bimhpH2pM";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPnjfceKuHNQu7S4eYFN1FqgzMqiL7haNZMh2ZLhvuhK root@xu";
}
